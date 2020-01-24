const Foo = artifacts.require("Foo");
const Bar = artifacts.require("Bar");

module.exports = function(deployer) {
  deployer.deploy(Bar).then(function() {
    return deployer.deploy(Foo, Bar.address);
  });
};
