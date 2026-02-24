import nacl from "tweetnacl";

export const keyPairFromSecretKeyImpl = (secretKey) => () =>
  nacl.sign.keyPair.fromSecretKey(secretKey);
