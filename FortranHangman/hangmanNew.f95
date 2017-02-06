! Scott Collier
! 0893291
! Feb 1 2017
! CIS 3190 Assignment 1
! This is a re-engineering/updateing of a fortran program into fortran 95
! This program is a playable hangman game.
! The program selects from a list of words and you have to guess the letters of the word
! After 10 incorrect letters you fail and the man is hanged

! Program: hangman
! This is the main of the program. It creates the variables used and manages picking the word to find

program hangman

    ! Declaration of variables
    integer :: i, j, continueFlag
    integer, dimension(50) :: usedWords
    character * 20, dimension(50) :: wordList
    character * 20 :: pickedWord

    ! Definition of word list
    data wordlist/ 'gum','sin','for','cry','lug','bye','fly','ugly', &
    & 'each','from','work','talk','with','self', &
    & 'pizza','thing','feign','fiend','elbow','fault', &
    & 'dirty','budget','spirit','quaint','maiden', &
    & 'escort','pickax','example','tension','quinine', &
    & 'kidney','replica','sleeper','triangle', &
    & 'kangaroo','mahogany','sergeant','sequence', &
    & 'moustache','dangerous','scientist','different', &
    & 'quiescent','magistrate','erroneously', &
    & 'loudspeaker','phytotoxic','matrimonial', &
    & 'parasympathomimetic','thigmotropism'/

    print *, "THE GAME OF HANGMAN"
    print *, "Lets play hangman."
    print *, "I'll pick a word and you have to guess letters to fill it in."
    print *, "Be careful though. Too many wrong guessed will hang the man."

    ! Initialize the array designating which words have been used
    do i=1,50
        usedWords(i) = 0
    end do

    ! Initialize variable used to determine the user wants to play
    continueFlag = 0

    ! While the user wants to play, randomly pick a unused word and plays hangman
    do while (continueFlag == 0)

        ! Generate random number using the system clock. It will be different every time
        call system_clock(i)
        j = (mod(i,50) + 1)

        ! If the word is unused, play hangman with it
        if (usedWords(j) == 0) then
            pickedWord = wordList(j)
            usedWords(j) = 1
            call playHangman(pickedWord)
        end if

        ! The user has either won or lost, ask if they want to continue
        call continueGame(continueFlag)

    end do

    print *, "Thank you for playing"
end program hangman



! subroutine: initialize
! This subroutine initializes aspects of the program and refreshes them between games
! It is passes the drawSpace, printedWord, and guessedLetters variables

subroutine initialize(drawSpace, printedWord, guessedLetters)

    integer :: i, j
    character, dimension(12,12):: drawSpace
    character, dimension(20) :: printedWord
    character, dimension(26) :: guessedLetters

    ! Initialize draw-space where the man is drawn
    do i=1,12
        do j=1,12
            drawSpace(i,j) = " "
        end do
        drawSpace(i,1) = "X"
    end do
    do i=1,7
        drawSpace(1,i) = "X"
    end do
    drawSpace(2,7) = "X"


    ! Initialize chosen-word string
    do i=1,20
        printedWord(i) = "-"
    end do

    ! Initialize guessed-letters array
    do i=1,26
        guessedLetters(i) = " "
    end do

end subroutine initialize



! subroutine: playHangman
! This subroutine manages the hangman game, including picking letters, hanging the man, and determining if you win or lose
! It is passed the pickedWord

subroutine playHangman(pickedWord)

    ! Declaration of variables
    integer :: i, wordLength, acceptFlag, finishFlag, hangmanStatus
    character, dimension(12,12):: drawSpace
    character, dimension(20) :: printedWord
    character, dimension(26) :: guessedLetters
    character * 20 :: pickedWord
    character :: inputLetter

    ! set wordLength to the length of the picked word
    wordLength = len_trim(pickedWord)

    ! Initialize finishFlag which determines if the game is over, and hangmanStatus which determines the hangman's status
    finishFlag = 0
    hangmanStatus = 0

    ! Initialize used variables
    call initialize(drawSpace, printedWord, guessedLetters)

    ! This while loop runs the game until it is finished
    do while(finishFlag == 0)

        ! Print the amount of the word the user has filled in, and print the guessed letters
        print *, (printedWord(i), i=1,wordLength)
        print *, "The Letters you have picked are:"
        write (*, '(A)' ,advance ="no") ' '
        do i=1,26
            if(guessedLetters(i) /= ' ') then
                write (*, '(A)' ,advance ="no") guessedLetters(i)
                write (*, '(A)' ,advance ="no") ', '
            end if
        end do

        print *, ""
        print *, "Please guess a letter"

        ! Read user input
        read (*,*) inputLetter
        acceptFlag = 0

        ! Call this subroutine to check if the user input a letter
        call checkIfLetter(inputLetter, acceptFlag)

        ! The chosen letter is accepted
        if (acceptFlag == 1) then
            
            ! Fill in the word if it contains the letter
            acceptFlag = 0
            do i=1,wordLength
                if(pickedWord(i:i) == inputLetter) then
                    printedWord(i) = inputLetter
                    acceptFlag = 1
                end if
            end do

            ! Add the used letter to the guessed-letters list
            do i=1,26
                if(guessedLetters(i) == " ") then
                    guessedLetters(i) = inputLetter
                    exit
                else if (guessedLetters(i) == inputLetter) then
                    print *, "You have already used this letter"
                    acceptFlag = 1
                    exit
                end if
            end do

            ! If the guessed letter isn't in the word, update the hangman and print to screen
            if (acceptFlag == 0) then
                print *, "That letter is incorrect."
                call setManStatus(hangmanStatus, drawSpace, pickedWord)
            end if

            ! If the man has been hung, exit the game            
            if (hangmanStatus == 10) exit


        ! The chosen letter isnt accepted
        else
            print *, "Entered character is not a letter."

        end if

        ! Check to see if the word is finished, and give signal to end if it is        
        finishFlag = 1
        do i=1,wordLength
            if (printedWord(i) == '-') then
                finishFlag = 0
            end if
        end do
    end do

    ! Print a congradulatory message detailing how many letters were guessed
    if (finishFlag == 1) then
        print *, "Word Finished"
        do i=1,26
            if (guessedLetters(i) == " ") exit
        end do
        i = i - 1
        write (*, '(A)' ,advance ="no") " It took you "
        write (*,"(I1)", advance = "no") i
        write (*, '(A)') " guesses."
    end if

end subroutine playHangman



! subroutine: checkIfLetter
! This subroutine takes in the user's input and checks to see if it is a letter
! It is passes the inputLetter and an acceptFlag to return a value with

subroutine checkIfLetter(inputLetter, acceptFlag)

    ! Declaration of variables
    integer :: i, acceptFlag
    character :: inputLetter
    character, dimension(52) :: acceptedLetters

    ! A list of the usable letters for comparison
    ! If index is odd it's capital, if even it's lowercase
    !    Used for converting capital to lowercase

    data acceptedLetters/ 'A', 'a', 'B', 'b', 'C', 'c', 'D', 'd', &
    & 'E', 'e', 'F', 'f', 'G', 'g', 'H', 'h', 'I', 'i', 'J', 'j', &
    & 'K', 'k', 'L', 'l', 'M', 'm', 'N', 'n', 'O', 'o', 'P', 'p', &
    & 'Q', 'q', 'R', 'r', 'S', 's', 'T', 't', 'U', 'u', 'V', 'v', &
    & 'W', 'w', 'X', 'x', 'Y', 'y', 'Z', 'z' /

    ! Check if read character is a letter
    do i=1,52 
        if (inputLetter == acceptedLetters(i)) then
            acceptFlag = 1

            ! If input is capital, convert to lowercase
            if(mod(i,2) .NE. 0) then
                inputLetter = acceptedLetters(i+1)
            end if
         end if
    end do

end subroutine checkIfLetter



! subroutine: setManStatus
! This subroutine draws the hangman given its status, and will print the lose message
! It is passes the hangman's status, the drawSpace, and the picked word

subroutine setManStatus(hangmanStatus, drawSpace, pickedWord)

    ! Declaration of variables
    integer :: hangmanStatus, i, j
    character * 20 :: pickedWord
    character, dimension(12,12):: drawSpace

    ! This switch/case statement manages drawing the hangman based in the status variable

    select case (hangmanStatus)

        ! First mistake, draw head
        case(0)
            print *, "First we draw a head."
            do i=6,8
                drawSpace(3,i) = "-"
                drawSpace(5,i) = "-"
            end do
            drawSpace(4,5) = "("; drawSpace(4,9) = ")"; drawSpace(4,6) = "."; drawSpace(4,8) = "."

        ! Second mistake, draw body
        case(1)
            print *, "Now we draw a body."
            do i=6,9
                drawSpace(i,7) = "X"
            end do

        ! Third mistake, draw first arm
        case(2)
            print *, "Next we draw an arm."
            do i=4,7
                drawSpace(i,i-1) = "\"
            end do

        ! Fourth mistake, draw second arm
        case(3)
            print *, "This time it's the other arm."
            do i=4,7
                drawSpace(i,15-i) = "/"
            end do

        ! Fifth mistake, draw first leg
        case(4)
            print *, "Now, let's draw the right leg."
            drawSpace(10,6) = "/"; drawSpace(11,5) = "/"

        ! Sixth mistake, draw second leg
        case(5)
            print *, "This time we draw the left leg."
            drawSpace(10,8) = "\"; drawSpace(11,9) = "\"

        ! Seventh mistake, draw fisrt hand
        case(6)
            print *, "Now we put up a hand."
            drawSpace(3,11) = "\"

        ! Eighth mistake, draw second hand
        case(7)
            print *, "Next the other hand."
            drawSpace(3,3) = "/"

        ! Ninth mistake, draw first foot
        case(8)
            print *, "Now we draw one foot."
            drawSpace(12,10) = "\"; drawSpace(12,11) = "-"

        ! Tenth mistake, draw second foot, user loses
        case(9)
            print *, "Here's the other foot -- You're hung!!."
            drawSpace(12,3) = "-"; drawSpace(12,4) = "/"

    end select

    ! print hangman to screen
    do i = 1,12
        write (*,*) (drawSpace(i,j), j=1,12)
    end do

    ! status indicates the user lost, print what the word was
    if (hangmanStatus == 9) then
        print *, "Sorry, you loose. The word was ", pickedWord
    end if

    hangmanStatus = hangmanStatus + 1

end subroutine setManStatus



! subroutine: continueGame
! This subroutine aks the user if they want to continue playing, and returns they answer
! It is passes the continue flag for their answer

subroutine continueGame(continueFlag)

    ! Declaration of variables
    integer :: i, continueFlag
    character :: userInput

    ! Ask the  user if they would like to contine playing, read their input, and return appropriate value
    print *, "Would you like to continue playing? (Y/N)"
    i = 0
    do while (i == 0)
        read (*,*) userInput

        if ((userInput == 'y') .OR. (userInput == 'Y')) then
            continueFlag = 0
            i = 1

        else if ((userInput == 'n') .OR. (userInput == 'N')) then
            continueFlag = 1
            i = 1

        else
            print *, "Please enter Y or N"

        end if
    end do

end subroutine continueGame