(* Initialize random number generator *)
let () = Random.self_init ()

(* Helper: Convert a card value to its display string *)
let card_to_string value =
  if value < 11 then string_of_int value
  else
    match value with
    | 11 -> "JACK"
    | 12 -> "QUEEN"
    | 13 -> "KING"
    | 14 -> "ACE"
    | _ -> "?"  (* Should not happen *)

(* Draw a card with value between 2 and 14 inclusive *)
let draw_card () = Random.int 13 + 2

(* Function to draw two cards such that the first is less than the second *)
let rec draw_two_cards () =
  let a = draw_card () in
  let b = draw_card () in
  if a >= b then draw_two_cards () else (a, b)

(* Read an integer from user input, with a prompt *)
let rec read_int prompt =
  print_string prompt;
  try int_of_string (read_line ()) with
  | Failure _ ->
      print_endline "Invalid input. Please enter a number.";
      read_int prompt

(* Read a string from user input, with a prompt *)
let read_string prompt =
  print_string prompt;
  read_line ()

(* The main game loop *)
let rec game_loop money =
  (* If money is 0, the game is over *)
  if money <= 0 then (
    print_endline "\nSORRY, FRIEND, BUT YOU BLEW YOUR WAD.";
    let answer = String.uppercase_ascii (read_string "TRY AGAIN (YES OR NO)? ") in
    if answer = "YES" then game_loop 100
    else print_endline "O.K., HOPE YOU HAD FUN!"
  )
  else (
    Printf.printf "\nYOU NOW HAVE %d DOLLARS.\n\n" money;
    (* Draw two cards for the round *)
    print_endline "HERE ARE YOUR NEXT TWO CARDS:";
    let (a, b) = draw_two_cards () in
    Printf.printf "%s   %s\n\n" (card_to_string a) (card_to_string b);
    
    (* Prompt the player for a bet *)
    let rec get_bet () =
      let bet = read_int "WHAT IS YOUR BET? " in
      if bet = 0 then (
        print_endline "CHICKEN!!\n";
        (* If bet is 0, skip to a new round without drawing the third card *)
        game_loop money;
        (* After game_loop returns, we stop processing here *)
        bet  (* dummy return; won’t be used *)
      )
      else if bet > money then (
        Printf.printf "SORRY, MY FRIEND, BUT YOU BET TOO MUCH. YOU HAVE ONLY %d DOLLARS TO BET.\n" money;
        get_bet ()
      )
      else bet
    in
    let bet = get_bet () in

    (* Draw the third card *)
    let rec draw_third_card () =
      let c = draw_card () in
      (* In the BASIC code, there is a loop to ensure card drawn is valid.
         Since our draw_card already returns a value in the proper range, we can simply return it. *)
      c
    in
    let c = draw_third_card () in
    Printf.printf "\nThe third card is: %s\n" (card_to_string c);

    (* Determine win/loss *)
    if c > a && c < b then (
      print_endline "YOU WIN!!!";
      game_loop (money + bet)
    )
    else (
      print_endline "SORRY, YOU LOSE";
      let new_money = money - bet in
      game_loop new_money
    )
  )

(* Print title and instructions *)
let print_instructions () =
  print_endline "\n           ACEY DUCEY CARD GAME";
  print_endline "      CREATIVE COMPUTING  MORRISTOWN, NEW JERSEY\n";
  print_endline "ACEY-DUCEY IS PLAYED IN THE FOLLOWING MANNER:";
  print_endline "THE DEALER (COMPUTER) DEALS TWO CARDS FACE UP.";
  print_endline "YOU HAVE AN OPTION TO BET OR NOT BET DEPENDING";
  print_endline "ON WHETHER OR NOT YOU FEEL THE CARD WILL HAVE";
  print_endline "A VALUE BETWEEN THE FIRST TWO.";
  print_endline "IF YOU DO NOT WANT TO BET, INPUT A 0.\n"

(* Main entry point *)
let () =
  print_instructions ();
  game_loop 100
