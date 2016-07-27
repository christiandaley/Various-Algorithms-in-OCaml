open Bigarray;;
open Array1;;
open Printf;;

let uint32_t = Bigarray.Int32;;
let uint8_t = Bigarray.Int8_unsigned;;
let layout = Bigarray.C_layout;;

let k = Array1.of_array uint32_t layout
[|  0x428a2f98l; 0x71374491l; 0xb5c0fbcfl; 0xe9b5dba5l; 0x3956c25bl; 0x59f111f1l; 0x923f82a4l; 0xab1c5ed5l;
    0xd807aa98l; 0x12835b01l; 0x243185bel; 0x550c7dc3l; 0x72be5d74l; 0x80deb1fel; 0x9bdc06a7l; 0xc19bf174l;
    0xe49b69c1l; 0xefbe4786l; 0x0fc19dc6l; 0x240ca1ccl; 0x2de92c6fl; 0x4a7484aal; 0x5cb0a9dcl; 0x76f988dal;
    0x983e5152l; 0xa831c66dl; 0xb00327c8l; 0xbf597fc7l; 0xc6e00bf3l; 0xd5a79147l; 0x06ca6351l; 0x14292967l;
    0x27b70a85l; 0x2e1b2138l; 0x4d2c6dfcl; 0x53380d13l; 0x650a7354l; 0x766a0abbl; 0x81c2c92el; 0x92722c85l;
    0xa2bfe8a1l; 0xa81a664bl; 0xc24b8b70l; 0xc76c51a3l; 0xd192e819l; 0xd6990624l; 0xf40e3585l; 0x106aa070l;
    0x19a4c116l; 0x1e376c08l; 0x2748774cl; 0x34b0bcb5l; 0x391c0cb3l; 0x4ed8aa4al; 0x5b9cca4fl; 0x682e6ff3l;
    0x748f82eel; 0x78a5636fl; 0x84c87814l; 0x8cc70208l; 0x90befffal; 0xa4506cebl; 0xbef9a3f7l; 0xc67178f2l |];;

let words_per_block = 16
and bytes_per_block = 64
and bytes_per_word = 4
and rounds_per_block = 64;;

let rotr x n =
  Int32.logor (Int32.shift_right_logical x n) (Int32.shift_left x (32 - n));;

let ch x y z = 
  Int32.logxor (Int32.logand x y) (Int32.logand (Int32.lognot x) z);;

let maj x y z =
  Int32.logxor (Int32.logxor (Int32.logand x y) (Int32.logand x z)) (Int32.logand y z);;

let bigE0 x =
  Int32.logxor (Int32.logxor (rotr x 2) (rotr x 13)) (rotr x 22);;

let bigE1 x =
  Int32.logxor (Int32.logxor (rotr x 6) (rotr x 11)) (rotr x 25);;

let littleE0 x =
  Int32.logxor (Int32.logxor (rotr x 7) (rotr x 17)) (Int32.shift_right_logical x 3);;

let littleE1 x =
  Int32.logxor (Int32.logxor (rotr x 17) (rotr x 19)) (Int32.shift_right_logical x 10);;

let init_hash hash =
  hash.{0} <- 0x6a09e667l;
  hash.{1} <- 0xbb67ae85l;
  hash.{2} <- 0x3c6ef372l;
  hash.{3} <- 0xa54ff53al;
  hash.{4} <- 0x510e527fl;
  hash.{5} <- 0x9b05688cl;
  hash.{6} <- 0x1f83d9abl;
  hash.{7} <- 0x5be0cd19l;;

let init_padding msg length n_blocks =
  let n =
    if (length mod bytes_per_block) >= 56 then
      length + bytes_per_block
    else
      length
  in let num_bytes = n + bytes_per_block - (n mod bytes_per_block) in
  let num_bits = num_bytes * 8
  and num_blocks = num_bytes / bytes_per_block in
  let blocks = Array1.create uint32_t layout (num_blocks * words_per_block)
  and padded_msg = Array1.create uint8_t layout num_bytes in
  Array1.fill padded_msg 0;
  for i = 0 to (length - 1) do
    padded_msg.{i} <- msg.{i};
  done;
  padded_msg.{length} <- 0x80;
  
  padded_msg.{num_bytes - 8} <- (num_bits lsr 56) land 0xff;
  padded_msg.{num_bytes - 7} <- (num_bits lsr 48) land 0xff;
  padded_msg.{num_bytes - 6} <- (num_bits lsr 40) land 0xff;
  padded_msg.{num_bytes - 5} <- (num_bits lsr 32) land 0xff;
  padded_msg.{num_bytes - 4} <- (num_bits lsr 24) land 0xff;
  padded_msg.{num_bytes - 3} <- (num_bits lsr 16) land 0xff;
  padded_msg.{num_bytes - 2} <- (num_bits lsr 8) land 0xff;
  padded_msg.{num_bytes - 1} <- num_bits land 0xff;
  
  for i = 0 to (num_blocks - 1) do
    for j = 0 to (words_per_block - 1) do
      let t0 = padded_msg.{(i * bytes_per_block) + (j * bytes_per_word)}
      and t1 = padded_msg.{(i * bytes_per_block) + (j * bytes_per_word) + 1}
      and t2 = padded_msg.{(i * bytes_per_block) + (j * bytes_per_word) + 2}
      and t3 = padded_msg.{(i * bytes_per_block) + (j * bytes_per_word) + 3} in
      let w0 = Int32.shift_left (Int32.of_int t0) 24
      and w1 = Int32.shift_left (Int32.of_int t1) 16
      and w2 = Int32.shift_left (Int32.of_int t2) 8
      and w3 = Int32.of_int t3 in
      blocks.{i * words_per_block + j} <- Int32.logxor (Int32.logxor (Int32.logxor w0 w1) w2) w3;
    done;
  done;
  n_blocks := num_blocks;
  blocks;;
  
let expand_blocks blocks w n =
  for i = 0 to (rounds_per_block - 1) do
    let temp =
      if i < 16 then
        blocks.{(n * words_per_block) + i}
      else
        Int32.add (Int32.add (Int32.add (littleE1 w.{i - 2}) w.{i - 7}) (littleE0 w.{i - 15})) w.{i - 16}
    in w.{i} <- temp;
  done;;
         

let sha256_hash msg length hash =
  init_hash hash;
  let n_blocks = ref 0 in
  let blocks = init_padding msg length n_blocks
  and w = Array1.create uint32_t layout rounds_per_block in
  let a = ref 0l
  and b = ref 0l
  and c = ref 0l
  and d = ref 0l
  and e = ref 0l
  and f = ref 0l
  and g = ref 0l
  and h = ref 0l in

  for i = 1 to (!n_blocks - 1) do
    a := hash.{0};
    b := hash.{1};
    c := hash.{2};
    d := hash.{3};
    e := hash.{4};
    f := hash.{5};
    g := hash.{6};
    h := hash.{7};
    expand_blocks blocks w (i - 1);
    for round = 0 to (rounds_per_block - 1) do
      let t1 = Int32.add (Int32.add (Int32.add (Int32.add !h (bigE1 !e)) (ch !e !f !g)) k.{round}) w.{round}
      and t2 = Int32.add (bigE0 !a) (maj !a !b !c) in
      h := !g;
      g := !f;
      f := !e;
      e := Int32.add !d t1;
      d := !c;
      c := !b;
      b := !a;
      a := Int32.add t1 t2;
    done;

    hash.{0} <- Int32.add hash.{0} !a;
    hash.{1} <- Int32.add hash.{1} !b;
    hash.{2} <- Int32.add hash.{2} !c;
    hash.{3} <- Int32.add hash.{3} !d;
    hash.{4} <- Int32.add hash.{4} !e;
    hash.{5} <- Int32.add hash.{5} !f;
    hash.{6} <- Int32.add hash.{6} !g;
    hash.{7} <- Int32.add hash.{7} !h;


  done;;