export const encryptImpl = (passphrase) => (plaintext) => (onError) => (onSuccess) => () => {
  (async () => {
    try {
      const enc = new TextEncoder();
      const salt = crypto.getRandomValues(new Uint8Array(16));
      const iv = crypto.getRandomValues(new Uint8Array(12));
      const keyMaterial = await crypto.subtle.importKey(
        "raw",
        enc.encode(passphrase),
        "PBKDF2",
        false,
        ["deriveKey"]
      );
      const key = await crypto.subtle.deriveKey(
        { name: "PBKDF2", salt, iterations: 100000, hash: "SHA-256" },
        keyMaterial,
        { name: "AES-GCM", length: 256 },
        false,
        ["encrypt"]
      );
      const ciphertext = await crypto.subtle.encrypt(
        { name: "AES-GCM", iv },
        key,
        plaintext
      );
      const combined = new Uint8Array(
        salt.length + iv.length + ciphertext.byteLength
      );
      combined.set(salt, 0);
      combined.set(iv, salt.length);
      combined.set(new Uint8Array(ciphertext), salt.length + iv.length);
      const base64 = btoa(String.fromCharCode(...combined));
      onSuccess(base64)();
    } catch (e) {
      onError(e)();
    }
  })();
  return () => {};
};

export const decryptImpl = (passphrase) => (base64str) => (onError) => (onSuccess) => () => {
  (async () => {
    try {
      const enc = new TextEncoder();
      const raw = Uint8Array.from(atob(base64str), (c) => c.charCodeAt(0));
      const salt = raw.slice(0, 16);
      const iv = raw.slice(16, 28);
      const ciphertext = raw.slice(28);
      const keyMaterial = await crypto.subtle.importKey(
        "raw",
        enc.encode(passphrase),
        "PBKDF2",
        false,
        ["deriveKey"]
      );
      const key = await crypto.subtle.deriveKey(
        { name: "PBKDF2", salt, iterations: 100000, hash: "SHA-256" },
        keyMaterial,
        { name: "AES-GCM", length: 256 },
        false,
        ["decrypt"]
      );
      const plaintext = await crypto.subtle.decrypt(
        { name: "AES-GCM", iv },
        key,
        ciphertext
      );
      onSuccess(new Uint8Array(plaintext))();
    } catch (e) {
      onError(e)();
    }
  })();
  return () => {};
};
