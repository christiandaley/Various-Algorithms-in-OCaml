(* Mersenne Twister *)

open Int32;;
open Int64;;

let n_32 = 624
and m_32 = 397
and a_32 = (Int32.of_int 0x9908b0df)
and lm_32 = (Int32.of_int 0x7fffffff)
and um_32 = (Int32.shift_left (Int32.of_int 1) 30)
and u_32 = 11
and s_32 = 7
and t_32 = 15
and d_32 = (Int32.of_int 0xffffffff)
and b_32 = (Int32.of_int 0x9d2c5680)
and c_32 = (Int32.of_int 0xefc60000)
and l_32 = 18
and f_32 = (Int32.of_int 1812433253);;

let add32 x y =
     Int32.add x y;;

let mul32 x y = 
     Int32.mul x y;;

let and32 x y = 
     Int32.logand x y;;

let xor32 x y =
     Int32.logxor x y;;

let or32 x y =
     Int32.logor x y;;

let shift_right32 x y =
     Int32.shift_right_logical x y;;

let shift_left32 x y = 
     Int32.shift_left x y;;

type mt_state_32 = {
    buffer : int32 array;
    mutable loc : int;
}

let new_state_32 () = { buffer = Array.make n_32 (Int32.of_int 0); loc = n_32 + 1; };;

let init_32 seed =
    let state = new_state_32 () in
    Array.set state.buffer 0 seed;
    for i = 1 to (n_32 - 1) do
    let x0 = Array.get state.buffer (i - 1) in
        let x1 = (add32 (mul32 f_32 (xor32 x0 (shift_right32 x0 30))) (Int32.of_int i)) in
            Array.set state.buffer i x1;
    done;
    state;;

let rand_32 state =
    let y0 =
    if state.loc > n_32 then (
        for i = 0 to (n_32 - 1) do
            let x0 = (Array.get state.buffer i)
            and x1 = (Array.get state.buffer ((i+1) mod n_32)) in
            let x = (or32 (and32 x0 um_32) (and32 x1 lm_32)) in
            let xa = if (and32 x (Int32.of_int 1)) == (Int32.of_int 1) then
                (xor32 (shift_right32 x 1) x)
                else
                shift_right32 x 1
            in
                Array.set state.buffer i (xor32 (Array.get state.buffer ((i + m_32) mod n_32)) xa);

    done;
    state.loc <- 1;
    Array.get state.buffer 0;
    ) else (
    state.loc <- state.loc + 1;
        Array.get state.buffer (state.loc - 1)
    )
    in
        let y1 = xor32 y0 (and32 (shift_right32 y0 u_32) d_32) in
        let y2 = xor32 y1 (and32 (shift_left32 y1 s_32) b_32) in
        let y3 = xor32 y2 (and32 (shift_left32 y2 t_32) c_32) in
        xor32 y3 (shift_right32 y3 l_32);;
