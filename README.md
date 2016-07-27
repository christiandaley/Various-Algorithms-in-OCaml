# Various-Algorithms-in-OCaml
Mersenne Twister, AES-128, and SHA-256 implemented in OCaml
Instructions for use:

To encrypt data using AES, both the key and plaintext should be a Bigarray containing 16 unsigned 8-bit numbers. First call aes_key_init to set up the key. Once a key is setup you do not need to call aes_key_init again unless you want to change keys. Then simply call aes_128_enc pt ct to encrypt the data. "pt" is the plaintext and "ct" is the resulting ciphertext. Both should be Bigarrays containing 16 unsigned 8 bit numbers.

To use the mersenne twister, first call init_32 and supply a seed. This function returns the internal state of the algorithm. Then call rand_32, supplying the state. This function will return a random number and update the internal state accordingly.

To use SHA-256 simply use the function "hash". The inputs are "msg", a Bigarray of unsigned 8-bit integers, "length", the length of msg in bytes, and "hash", a Bigarray of 8 32-bit integers where the resulting hash will be stored.
