open Int32;;

let s_box =
  [|0x63; 0x7C; 0x77; 0x7B; 0xF2; 0x6B; 0x6F; 0xC5; 0x30; 0x01; 0x67; 0x2B; 0xFE; 0xD7; 0xAB; 0x76;
    0xCA; 0x82; 0xC9; 0x7D; 0xFA; 0x59; 0x47; 0xF0; 0xAD; 0xD4; 0xA2; 0xAF; 0x9C; 0xA4; 0x72; 0xC0;
    0xB7; 0xFD; 0x93; 0x26; 0x36; 0x3F; 0xF7; 0xCC; 0x34; 0xA5; 0xE5; 0xF1; 0x71; 0xD8; 0x31; 0x15;
    0x04; 0xC7; 0x23; 0xC3; 0x18; 0x96; 0x05; 0x9A; 0x07; 0x12; 0x80; 0xE2; 0xEB; 0x27; 0xB2; 0x75;
    0x09; 0x83; 0x2C; 0x1A; 0x1B; 0x6E; 0x5A; 0xA0; 0x52; 0x3B; 0xD6; 0xB3; 0x29; 0xE3; 0x2F; 0x84;
    0x53; 0xD1; 0x00; 0xED; 0x20; 0xFC; 0xB1; 0x5B; 0x6A; 0xCB; 0xBE; 0x39; 0x4A; 0x4C; 0x58; 0xCF;
    0xD0; 0xEF; 0xAA; 0xFB; 0x43; 0x4D; 0x33; 0x85; 0x45; 0xF9; 0x02; 0x7F; 0x50; 0x3C; 0x9F; 0xA8;
    0x51; 0xA3; 0x40; 0x8F; 0x92; 0x9D; 0x38; 0xF5; 0xBC; 0xB6; 0xDA; 0x21; 0x10; 0xFF; 0xF3; 0xD2;
    0xCD; 0x0C; 0x13; 0xEC; 0x5F; 0x97; 0x44; 0x17; 0xC4; 0xA7; 0x7E; 0x3D; 0x64; 0x5D; 0x19; 0x73;
    0x60; 0x81; 0x4F; 0xDC; 0x22; 0x2A; 0x90; 0x88; 0x46; 0xEE; 0xB8; 0x14; 0xDE; 0x5E; 0x0B; 0xDB;
    0xE0; 0x32; 0x3A; 0x0A; 0x49; 0x06; 0x24; 0x5C; 0xC2; 0xD3; 0xAC; 0x62; 0x91; 0x95; 0xE4; 0x79;
    0xE7; 0xC8; 0x37; 0x6D; 0x8D; 0xD5; 0x4E; 0xA9; 0x6C; 0x56; 0xF4; 0xEA; 0x65; 0x7A; 0xAE; 0x08;
    0xBA; 0x78; 0x25; 0x2E; 0x1C; 0xA6; 0xB4; 0xC6; 0xE8; 0xDD; 0x74; 0x1F; 0x4B; 0xBD; 0x8B; 0x8A;
    0x70; 0x3E; 0xB5; 0x66; 0x48; 0x03; 0xF6; 0x0E; 0x61; 0x35; 0x57; 0xB9; 0x86; 0xC1; 0x1D; 0x9E;
    0xE1; 0xF8; 0x98; 0x11; 0x69; 0xD9; 0x8E; 0x94; 0x9B; 0x1E; 0x87; 0xE9; 0xCE; 0x55; 0x28; 0xDF;
    0x8C; 0xA1; 0x89; 0x0D; 0xBF; 0xE6; 0x42; 0x68; 0x41; 0x99; 0x2D; 0x0F; 0xB0; 0x54; 0xBB; 0x16 |];;

let inv_s_box =
  [|0x52; 0x09; 0x6A; 0xD5; 0x30; 0x36; 0xA5; 0x38; 0xBF; 0x40; 0xA3; 0x9E; 0x81; 0xF3; 0xD7; 0xFB;
    0x7C; 0xE3; 0x39; 0x82; 0x9B; 0x2F; 0xFF; 0x87; 0x34; 0x8E; 0x43; 0x44; 0xC4; 0xDE; 0xE9; 0xCB;
    0x54; 0x7B; 0x94; 0x32; 0xA6; 0xC2; 0x23; 0x3D; 0xEE; 0x4C; 0x95; 0x0B; 0x42; 0xFA; 0xC3; 0x4E;
    0x08; 0x2E; 0xA1; 0x66; 0x28; 0xD9; 0x24; 0xB2; 0x76; 0x5B; 0xA2; 0x49; 0x6D; 0x8B; 0xD1; 0x25;
    0x72; 0xF8; 0xF6; 0x64; 0x86; 0x68; 0x98; 0x16; 0xD4; 0xA4; 0x5C; 0xCC; 0x5D; 0x65; 0xB6; 0x92;
    0x6C; 0x70; 0x48; 0x50; 0xFD; 0xED; 0xB9; 0xDA; 0x5E; 0x15; 0x46; 0x57; 0xA7; 0x8D; 0x9D; 0x84;
    0x90; 0xD8; 0xAB; 0x00; 0x8C; 0xBC; 0xD3; 0x0A; 0xF7; 0xE4; 0x58; 0x05; 0xB8; 0xB3; 0x45; 0x06;
    0xD0; 0x2C; 0x1E; 0x8F; 0xCA; 0x3F; 0x0F; 0x02; 0xC1; 0xAF; 0xBD; 0x03; 0x01; 0x13; 0x8A; 0x6B;
    0x3A; 0x91; 0x11; 0x41; 0x4F; 0x67; 0xDC; 0xEA; 0x97; 0xF2; 0xCF; 0xCE; 0xF0; 0xB4; 0xE6; 0x73;
    0x96; 0xAC; 0x74; 0x22; 0xE7; 0xAD; 0x35; 0x85; 0xE2; 0xF9; 0x37; 0xE8; 0x1C; 0x75; 0xDF; 0x6E;
    0x47; 0xF1; 0x1A; 0x71; 0x1D; 0x29; 0xC5; 0x89; 0x6F; 0xB7; 0x62; 0x0E; 0xAA; 0x18; 0xBE; 0x1B;
    0xFC; 0x56; 0x3E; 0x4B; 0xC6; 0xD2; 0x79; 0x20; 0x9A; 0xDB; 0xC0; 0xFE; 0x78; 0xCD; 0x5A; 0xF4;
    0x1F; 0xDD; 0xA8; 0x33; 0x88; 0x07; 0xC7; 0x31; 0xB1; 0x12; 0x10; 0x59; 0x27; 0x80; 0xEC; 0x5F;
    0x60; 0x51; 0x7F; 0xA9; 0x19; 0xB5; 0x4A; 0x0D; 0x2D; 0xE5; 0x7A; 0x9F; 0x93; 0xC9; 0x9C; 0xEF;
    0xA0; 0xE0; 0x3B; 0x4D; 0xAE; 0x2A; 0xF5; 0xB0; 0xC8; 0xEB; 0xBB; 0x3C; 0x83; 0x53; 0x99; 0x61;
    0x17; 0x2B; 0x04; 0x7E; 0xBA; 0x77; 0xD6; 0x26; 0xE1; 0x69; 0x14; 0x63; 0x55; 0x21; 0x0C; 0x7D |];;

let r_con = 
  [| 0x00; 0x01; 0x02; 0x04; 0x08; 0x10; 0x20; 0x40; 0x80; 0x1b; 0x36 |];;

let round_keys = Array.make 176 0;;

let mult_x n =   
  if ((n land 0x80) != 0) then
    ((n lsl 1) lxor 0x1b) land 0xff
  else
    (n lsl 1) land 0xff;;

let gf_mult x y =
  let result = ref 0
  and x_ref = ref x
  and y_ref = ref y in
  while (!x_ref != 0) do
    let z =  
      if ((!x_ref land 1) != 0) then
        !y_ref
      else
        0
    in result := !result lxor z;

    x_ref := (!x_ref lsr 1);
    y_ref := mult_x !y_ref;
  done;

  !result;;

let aes_key_init key =
  let word = Array.make 4 0 in
  for i = 0 to 15 do
    Array.set round_keys i (Array.get key i);
  done;

  for round = 1 to 10 do
    for i = 0 to 3 do
      Array.set word i (Array.get round_keys (16 * round - 4 + i));
    done;
    let temp = Array.get s_box (Array.get word 0) in
    Array.set word 0 ((Array.get s_box (Array.get word 1)) lxor (Array.get r_con round));
    Array.set word 1 (Array.get s_box (Array.get word 2));
    Array.set word 2 (Array.get s_box (Array.get word 3));
    Array.set word 3 temp;

    for i = 0 to 15 do
      let z =
        if i < 4 then
          (Array.get word i) lxor (Array.get round_keys (16 * (round - 1) + i))
        else
          (Array.get round_keys (16 * round + i - 4)) lxor (Array.get round_keys (16 * (round - 1) + i))
      in Array.set round_keys (16 * round + i) z;
    done;
  done;;

let add_round_key state round = 
  for i = 0 to 15 do
    Array.set state i ((Array.get state i) lxor (Array.get round_keys (round * 16 + i)))
  done;;

let sub_bytes state =
  for i = 0 to 15 do
    Array.set state i (Array.get s_box (Array.get state i))
  done;;

let shift_rows state = 
  let temp = ref (Array.get state 1) in
  Array.set state 1 (Array.get state 5);
  Array.set state 5 (Array.get state 9);
  Array.set state 9 (Array.get state 13);
  Array.set state 13 !temp;

  temp := Array.get state 2;
  Array.set state 2 (Array.get state 10);
  Array.set state 10 !temp;
  temp := Array.get state 6;
  Array.set state 6 (Array.get state 14);
  Array.set state 14 !temp;

  temp := Array.get state 15;
  Array.set state 15 (Array.get state 11);
  Array.set state 11 (Array.get state 7);
  Array.set state 7 (Array.get state 3);
  Array.set state 3 !temp;;

let mix_columns state =
  let output = Array.make 16 0
  and mult2 = Array.make 16 0
  and mult3 = Array.make 16 0 in
  for i = 0 to 15 do
    Array.set output i (Array.get state i);
    Array.set mult2 i (mult_x (Array.get state i));
    Array.set mult3 i (gf_mult (Array.get state i) 3);
  done;

  for i = 0 to 3 do
    Array.set output (i * 4) ((Array.get mult2 (i * 4)) lxor (Array.get mult3 (i * 4 + 1)) lxor (Array.get state (i * 4 + 2)) lxor (Array.get state (i * 4 + 3)));
    Array.set output (i * 4 + 1) ((Array.get state (i * 4)) lxor (Array.get mult2 (i * 4 + 1)) lxor (Array.get mult3 (i * 4 + 2)) lxor (Array.get state (i * 4 + 3)));
    Array.set output (i * 4 + 2) ((Array.get state (i * 4)) lxor (Array.get state (i * 4 + 1)) lxor (Array.get mult2 (i * 4 + 2)) lxor (Array.get mult3 (i * 4 + 3)));
    Array.set output (i * 4 + 3) ((Array.get mult3 (i * 4)) lxor (Array.get state (i * 4 + 1)) lxor (Array.get state (i * 4 + 2)) lxor (Array.get mult2 (i * 4 + 3))); 
  done;

  for i = 0 to 15 do
    Array.set state i (Array.get output i);
  done;;

let inv_sub_bytes state =
  for i = 0 to 15 do
    Array.set state i (Array.get inv_s_box (Array.get state i))
  done;;

let inv_shift_rows state =
  let temp = ref (Array.get state 13) in
  Array.set state 13 (Array.get state 9);
  Array.set state 9 (Array.get state 5);
  Array.set state 5 (Array.get state 1);
  Array.set state 1 !temp;

  temp := Array.get state 2;
  Array.set state 2 (Array.get state 10);
  Array.set state 10 !temp;
  temp := Array.get state 6;
  Array.set state 6 (Array.get state 14);
  Array.set state 14 !temp;

  temp := Array.get state 3;
  Array.set state 3 (Array.get state 7);
  Array.set state 7 (Array.get state 11);
  Array.set state 11 (Array.get state 15);
  Array.set state 15 !temp;;

let inv_mix_columns state =
  let mult9 = Array.make 16 0
  and mult11 = Array.make 16 0
  and mult13 = Array.make 16 0
  and mult14 = Array.make 16 0 in
  for i = 0 to 15 do
    Array.set mult9 i (gf_mult (Array.get state i) 9);
    Array.set mult11 i (gf_mult (Array.get state i) 11);
    Array.set mult13 i (gf_mult (Array.get state i) 13);
    Array.set mult14 i (gf_mult (Array.get state i) 14);
  done;

  for i = 0 to 3 do
    Array.set state (i * 4) ((Array.get mult14 (i * 4)) lxor (Array.get mult11 (i * 4 + 1)) lxor (Array.get mult13 (i * 4 + 2)) lxor (Array.get mult9 (i * 4 + 3)));
    Array.set state (i * 4 + 1) ((Array.get mult9 (i * 4)) lxor (Array.get mult14 (i * 4 + 1)) lxor (Array.get mult11 (i *4 + 2)) lxor (Array.get mult13 (i * 4 + 3)));
    Array.set state (i * 4 + 2) ((Array.get mult13 (i * 4)) lxor (Array.get mult9 (i * 4 + 1)) lxor (Array.get mult14 (i *4 + 2)) lxor (Array.get mult11 (i * 4 + 3)));
    Array.set state (i * 4 + 3) ((Array.get mult11 (i * 4)) lxor (Array.get mult13 (i * 4 + 1)) lxor (Array.get mult9 (i *4 + 2)) lxor (Array.get mult14 (i * 4 + 3))); 
  done;;

let expand_state input =
  let state = Array.make 16 0 in
  for i = 0 to 3 do
    Array.set state (i * 4) (Int32.to_int (Int32.logand (Array.get input i) (Int32.of_int 0xff)));
    Array.set state (i * 4 + 1) (Int32.to_int (Int32.logand (Int32.shift_right_logical (Array.get input i) 8) (Int32.of_int 0xff)));
    Array.set state (i * 4 + 2) (Int32.to_int (Int32.logand (Int32.shift_right_logical (Array.get input i) 16) (Int32.of_int 0xff)));
    Array.set state (i * 4 + 3) (Int32.to_int (Int32.logand (Int32.shift_right_logical (Array.get input i) 24) (Int32.of_int 0xff)));
  done;
  state;;

let shrink_state state = 
  let output = Array.make 4 (Int32.of_int 0) in
  for i = 0 to 3 do
    let t0 = Int32.of_int (Array.get state 0);
    and t1 = Int32.shift_left (Int32.of_int (Array.get state 1)) 8;
    and t2 = Int32.shift_left (Int32.of_int (Array.get state 2)) 16;
    and t3 = Int32.shift_left (Int32.of_int (Array.get state 3)) 24;
    in
    let z = Int32.logxor (Int32.logxor (Int32.logxor t0 t1) t2) t3 in
    Array.set output i z;
  done;
  output;;

let aes_128_enc pt ct key =
  let state = expand_state pt in
  aes_key_init key;
  add_round_key state 0;
  for round = 1 to 9 do
    sub_bytes state;
    shift_rows state;
    mix_columns state;
    add_round_key state round;
  done;

  sub_bytes state;
  shift_rows state;
  add_round_key state 10;
  let output = shrink_state state in
  for i = 0 to 3 do
    Array.set ct i (Array.get output i);
  done;;

let aes_128_dec ct pt key =
  let state = expand_state ct in
  aes_key_init key;
  add_round_key state 10;
  inv_shift_rows state;
  inv_sub_bytes state;
  
  for round = 9 to 1 do
    add_round_key state round;
    inv_mix_columns state;
    inv_shift_rows state;
    inv_sub_bytes state;
  done;
  
    add_round_key state 0;
    let output = shrink_state state in
    for i = 0 to 3 do
      Array.set pt i (Array.get output i);
    done;;