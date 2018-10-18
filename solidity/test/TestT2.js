const T2 = artifacts.require('./T2.sol');
const Notary = artifacts.require('./NotaryConnector.sol');

contract('T2', function (accounts) {
  let app;

  const alice   = accounts[1];
  const bob     = accounts[2];
  const charlie = accounts[3];

  beforeEach('Initialise contract', async () => {
    notary = await Notary.new();
    app = await T2.new(alice, bob, charlie, notary.address);

    // issue assets
    await app.issueAsset("EUR", 100, alice);
    await app.issueAsset("EUR", 100, bob);
    await app.issueAsset("GBP", 100, charlie);

    console.log(app.address);
  });

  it('Alice Commit First', async () => {
    await app.c2_commit("EUR", 100, bob, {from:alice});

    let result = await app.fetchCommitment(0);

    // check the commitment values are accurate
    assert.equal(result[0], 'EUR');
    assert.equal(web3.toDecimal(result[1]), 100);
    assert.equal(result[2], alice);
    assert.equal(result[3], bob);
    // check status is 0
    assert.equal(web3.toDecimal(result[4]), 0);

    // console.log(result[0], result[1].toString());
    // console.log(result[2].toString(), "to", result[3].toString());
    // console.log("Status", result[4].toString());
  });

  it('Bob Commit First', async () => {
    await app.c1_commit("EUR", 100, alice, {from:bob});

    let result = await app.fetchCommitment(0);

    // check the commitment values are accurate
    assert.equal(result[0], 'EUR');
    assert.equal(web3.toDecimal(result[1]), 100);
    assert.equal(result[2], bob);
    assert.equal(result[3], alice);
    // check status is 0
    assert.equal(web3.toDecimal(result[4]), 0);

    // console.log(result[0], result[1].toString());
    // console.log(result[2].toString(), "to", result[3].toString());
    // console.log("Status", result[4].toString());
  });

  it('Alice then Bob Commit', async () => {
    await app.c2_commit("EUR", 100, bob, {from:alice});
    await app.c1_commit("EUR", 100, alice, {from:bob});

    let c2 = await app.fetchCommitment(0);
    let c1 = await app.fetchCommitment(1);

    // check the commitment values are accurate
    assert.equal(c1[0], 'EUR');
    assert.equal(web3.toDecimal(c1[1]), 100);
    assert.equal(c1[2], bob);
    assert.equal(c1[3], alice);
    // check status is 0
    assert.equal(web3.toDecimal(c1[4]), 0);

    // check the commitment values are accurate
    assert.equal(c2[0], 'EUR');
    assert.equal(web3.toDecimal(c2[1]), 100);
    assert.equal(c2[2], alice);
    assert.equal(c2[3], bob);
    // check status is 0
    assert.equal(web3.toDecimal(c2[4]), 0);
  });

  it('Bob then Alice Commit', async () => {
    await app.c1_commit("EUR", 100, alice, {from:bob});
    await app.c2_commit("EUR", 100, bob, {from:alice});

    let c1 = await app.fetchCommitment(0);
    let c2 = await app.fetchCommitment(1);

    // check the commitment values are accurate
    assert.equal(c1[0], 'EUR');
    assert.equal(web3.toDecimal(c1[1]), 100);
    assert.equal(c1[2], bob);
    assert.equal(c1[3], alice);
    // check status is 0
    assert.equal(web3.toDecimal(c1[4]), 0);

    // check the commitment values are accurate
    assert.equal(c2[0], 'EUR');
    assert.equal(web3.toDecimal(c2[1]), 100);
    assert.equal(c2[2], alice);
    assert.equal(c2[3], bob);
    // check status is 0
    assert.equal(web3.toDecimal(c2[4]), 0);
  });

  it('Charlie First should fail', async () => {
    await app.c3_commit("EUR", 100, bob, {from:charlie});

    try {
      let result = await app.fetchCommitment(0);
    } catch (e) {
      // successfully failed
    }
  });

  it('Charlie Before Bob After Alice should fail', async () => {
    await app.c2_commit("EUR", 100, bob, {from:alice});
    await app.c3_commit("EUR", 100, bob, {from:charlie});

    try {
      let result = await app.fetchCommitment(0);
      let result1 = await app.fetchCommitment(1);
    } catch (e) {
      // successfully failed
    }
  });

  it('Alice, Bob then Charlie', async () => {
    await app.c2_commit("EUR", 10, bob, {from:alice});
    await app.c1_commit("EUR", 100, alice, {from:bob});
    await app.c3_commit("GBP", 100, bob, {from:charlie});

    let c2 = await app.fetchCommitment(0);
    let c1 = await app.fetchCommitment(1);
    let c3 = await app.fetchCommitment(2);

    // check the commitment values are accurate
    assert.equal(c1[0], 'EUR');
    assert.equal(web3.toDecimal(c1[1]), 100);
    assert.equal(c1[2], bob);
    assert.equal(c1[3], alice);
    assert.equal(web3.toDecimal(c1[4]), 1);

    // check the commitment values are accurate
    assert.equal(c2[0], 'EUR');
    assert.equal(web3.toDecimal(c2[1]), 10);
    assert.equal(c2[2], alice);
    assert.equal(c2[3], bob);
    assert.equal(web3.toDecimal(c2[4]), 1);

    // check the commitment values are accurate
    assert.equal(c3[0], 'GBP');
    assert.equal(web3.toDecimal(c3[1]), 100);
    assert.equal(c3[2], charlie);
    assert.equal(c3[3], bob);
    assert.equal(web3.toDecimal(c3[4]), 1);

    // check the balances have moved correctly
    let alice_balance = await app.fetchBalance("EUR", alice);
    let bob_eur_balance = await app.fetchBalance("EUR", bob);
    let bob_gbp_balance = await app.fetchBalance("GBP", bob);
    let charlie_balance = await app.fetchBalance("EUR", charlie);

    assert.equal(web3.toDecimal(alice_balance), 190, "Alice's balance is expected to be 190");
    assert.equal(web3.toDecimal(bob_eur_balance), 10);
    assert.equal(web3.toDecimal(bob_gbp_balance), 100);
    assert.equal(web3.toDecimal(charlie_balance), 0);
  });

  it('Alice, Bob then Timeout Charlie', async () => {
    await app.c2_commit("EUR", 100, bob, {from:alice});
    await app.c1_commit("EUR", 100, alice, {from:bob});
    await app.c3_timeout();

    let c2 = await app.fetchCommitment(0);
    let c1 = await app.fetchCommitment(1);

    // check the commitment values are accurate
    assert.equal(c1[0], 'EUR');
    assert.equal(web3.toDecimal(c1[1]), 100);
    assert.equal(c1[2], bob);
    assert.equal(c1[3], alice);
    assert.equal(web3.toDecimal(c1[4]), 2);

    // check the commitment values are accurate
    assert.equal(c2[0], 'EUR');
    assert.equal(web3.toDecimal(c2[1]), 100);
    assert.equal(c2[2], alice);
    assert.equal(c2[3], bob);
    assert.equal(web3.toDecimal(c2[4]), 2);

        // check the balances have moved correctly
    let alice_balance = await app.fetchBalance("EUR", alice);
    let bob_balance = await app.fetchBalance("EUR", bob);
    let charlie_balance = await app.fetchBalance("GBP", charlie);

    assert.equal(web3.toDecimal(alice_balance), 100);
    assert.equal(web3.toDecimal(bob_balance), 100);
    assert.equal(web3.toDecimal(charlie_balance), 100);
  });

})
