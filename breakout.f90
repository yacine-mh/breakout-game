! ============================================================
!  BREAKOUT - Terminal Breakout game in Fortran
! by mahiaoui yacine
! ============================================================
program breakout_game
    use iso_c_binding
    implicit none

    ! Windows C function interfaces
    interface
        integer(c_int) function c_kbhit() bind(C, name="_kbhit")
            import :: c_int
        end function

        integer(c_int) function c_getch() bind(C, name="_getch")
            import :: c_int
        end function

        ! Sleep via msvcrt (takes milliseconds)
        subroutine c_usleep(ms) bind(C, name="_sleep")
            import :: c_int
            integer(c_int), value :: ms
        end subroutine

        ! Instantaneous physical keyboard state from user32 (zero typematic delay)
        function GetAsyncKeyState(vKey) bind(C, name="GetAsyncKeyState")
            import :: c_short, c_int
            !GCC$ ATTRIBUTES STDCALL :: GetAsyncKeyState
            integer(c_short) :: GetAsyncKeyState
            integer(c_int), value :: vKey
        end function
    end interface

    ! Game constants
    integer, parameter :: FW      = 50
    integer, parameter :: FH      = 20
    integer, parameter :: BROWS   = 4
    integer, parameter :: BCOLS   = 10
    integer, parameter :: BW      = 5
    integer, parameter :: PW_INIT = 8

    ! Game state
    integer :: bricks(BROWS, BCOLS)
    integer :: px, pw_len
    real    :: bx, by
    integer :: vx, vy
    integer :: score, lives, remaining, next_bonus
    integer :: tick
    logical :: playing, victory

    ! Render buffer
    character(len=200) :: lbuf
    integer :: lpos

    call run()

contains

    ! --- Check physical key state instantly (zero typematic delay) ---
    function is_key_down(vKey) result(down)
        integer, intent(in) :: vKey
        logical :: down
        down = (GetAsyncKeyState(int(vKey, c_int)) < 0)
    end function

    ! --- Sleep delay yielding CPU (0% CPU usage) ---
    subroutine delay_ms(ms)
        integer, intent(in) :: ms
        call c_usleep(int(ms, c_int))
    end subroutine

    ! --- Line buffer helpers ---
    subroutine lb_clear()
        lpos = 0
    end subroutine

    subroutine lb_add(s)
        character(len=*), intent(in) :: s
        integer :: n
        n = len(s)
        if (lpos + n > 200) return
        lbuf(lpos + 1 : lpos + n) = s
        lpos = lpos + n
    end subroutine

    subroutine lb_out()
        if (lpos > 0) then
            write(*, '(A)') lbuf(1:lpos)
        else
            write(*, '(A)') ''
        end if
    end subroutine

    ! --- Terminal helpers ---
    subroutine cursor_home()
        write(*, '(A)', advance='no') achar(27) // '[H'
    end subroutine

    subroutine cursor_hide()
        write(*, '(A)', advance='no') achar(27) // '[?25l'
    end subroutine

    subroutine cursor_show()
        write(*, '(A)', advance='no') achar(27) // '[?25h'
    end subroutine

    subroutine screen_clear()
        write(*, '(A)', advance='no') achar(27) // '[2J'
        call cursor_home()
    end subroutine

    ! --- Main game flow ---
    subroutine run()
        integer :: k
        logical :: keep_running

        call cursor_hide()
        keep_running = .true.

        do while (keep_running)
            call init_state()
            call screen_clear()

            ! Title / start screen
            call render()
            write(*, '(A)') ''
            write(*, '(A)') '  Press any key to start...'
            k = c_getch()
            if (k == 0 .or. k == 224) k = c_getch()

            tick = 0

            ! Game loop (40 FPS input polling for instant paddle response)
            do while (playing)
                tick = tick + 1
                call handle_input()
                if (mod(tick, 2) == 0) then
                    call update()
                end if
                call render()
                call delay_ms(25)
            end do

            ! Show end screen with choice to replay or quit
            call end_screen(keep_running)
        end do

        call screen_clear()
        call cursor_show()
        write(*, '(A)') '  Thanks for playing Breakout! Goodbye.'
        write(*, '(A)') ''
    end subroutine

    ! --- Init ---
    subroutine init_state()
        bricks     = 1
        pw_len     = PW_INIT
        px         = FW / 2 - pw_len / 2
        bx         = real(FW / 2)
        by         = real(FH - 3)
        vx         = 1
        vy         = -1
        score      = 0
        lives      = 3
        remaining  = BROWS * BCOLS
        next_bonus = 100
        tick       = 0
        playing    = .true.
        victory    = .false.
    end subroutine

    ! --- Input (Instant) ---
    subroutine handle_input()
        integer :: k
        logical :: moved

        moved = .false.

        ! 1. Check physical key states in real-time (instant reaction)
        if (is_key_down(37) .or. is_key_down(65)) then ! Left Arrow or 'A'
            px = max(1, px - 1)
            moved = .true.
        else if (is_key_down(39) .or. is_key_down(68)) then ! Right Arrow or 'D'
            px = min(FW - pw_len + 1, px + 1)
            moved = .true.
        end if

        if (is_key_down(81) .or. is_key_down(27)) then ! 'Q' or Esc
            playing = .false.
        end if

        ! 2. Process and drain buffered keys (supports rapid tap without lag)
        do while (c_kbhit() /= 0)
            k = c_getch()
            if (k == 0 .or. k == 224) then
                k = c_getch()
                if (.not. moved) then
                    select case (k)
                    case (75) ! Left Arrow
                        px = max(1, px - 1)
                        moved = .true.
                    case (77) ! Right Arrow
                        px = min(FW - pw_len + 1, px + 1)
                        moved = .true.
                    end select
                end if
            else
                select case (k)
                case (ichar('a'), ichar('A'))
                    if (.not. moved) then
                        px = max(1, px - 1)
                        moved = .true.
                    end if
                case (ichar('d'), ichar('D'))
                    if (.not. moved) then
                        px = min(FW - pw_len + 1, px + 1)
                        moved = .true.
                    end if
                case (ichar('q'), ichar('Q'), 27)
                    playing = .false.
                end select
            end if
        end do
    end subroutine

    ! --- Update ---
    subroutine update()
        integer :: ibx, iby, br, bc, p_third

        bx = bx + real(vx)
        by = by + real(vy)
        ibx = nint(bx)
        iby = nint(by)

        ! Wall bounces
        if (ibx < 1) then
            vx = 1
            bx = 1.0
        end if
        if (ibx > FW) then
            vx = -1
            bx = real(FW)
        end if
        if (iby < 1) then
            vy = 1
            by = 1.0
        end if

        ! Ball fell out - lose life
        if (iby > FH) then
            lives = lives - 1
            if (lives <= 0) then
                playing = .false.
                return
            end if
            call render()
            call delay_ms(250)
            bx = real(px + pw_len / 2)
            by = real(FH - 3)
            vx = 1
            vy = -1
            return
        end if

        ibx = nint(bx)
        iby = nint(by)

        ! Paddle bounce (only when moving downward vy > 0)
        if (vy > 0 .and. iby == FH - 1 .and. ibx >= px .and.       &
            ibx <= px + pw_len - 1) then
            vy = -1
            by = real(FH - 2)

            ! Steer ball: left third bounces left, right third bounces right
            p_third = max(1, pw_len / 3)
            if (ibx < px + p_third) then
                vx = -1
            else if (ibx > px + pw_len - 1 - p_third) then
                vx = 1
            end if
            return
        end if

        ! Brick collision (bricks at rows 2..BROWS+1)
        ibx = nint(bx)
        iby = nint(by)
        if (iby >= 2 .and. iby <= BROWS + 1) then
            br = iby - 1
            bc = (ibx - 1) / BW + 1
            if (bc >= 1 .and. bc <= BCOLS) then
                if (bricks(br, bc) == 1) then
                    bricks(br, bc) = 0
                    remaining = remaining - 1
                    score = score + (BROWS - br + 1) * 10
                    vy = -vy

                    ! Facilitation: Bonus life every 100 points
                    if (score >= next_bonus) then
                        if (lives < 5) lives = lives + 1
                        next_bonus = next_bonus + 100
                    end if

                    ! Facilitation: Paddle widens when clearing the board
                    if (remaining <= 5) then
                        pw_len = 12
                    else if (remaining <= 15) then
                        pw_len = 10
                    end if
                    px = min(px, FW - pw_len + 1)

                    if (remaining <= 0) then
                        victory = .true.
                        playing = .false.
                    end if
                end if
            end if
        end if
    end subroutine

    ! --- Render ---
    subroutine render()
        integer :: row, col, br, bc, ibx, iby
        character(len=12) :: stmp

        call cursor_home()

        ibx = nint(bx)
        iby = nint(by)

        ! Header
        write(stmp, '(I0)') score
        call lb_clear()
        call lb_add(' BREAKOUT | Score: ')
        call lb_add(trim(stmp))
        write(stmp, '(I0)') lives
        call lb_add(' | Lives: ')
        call lb_add(trim(stmp))
        write(stmp, '(I0)') remaining
        call lb_add(' | Bricks: ')
        call lb_add(trim(stmp))
        if (pw_len > PW_INIT) then
            call lb_add(' [WIDE PADDLE]')
        end if
        call lb_add('             ')
        call lb_out()

        ! Top border
        call lb_clear()
        call lb_add('+')
        call lb_add(repeat('-', FW))
        call lb_add('+')
        call lb_out()

        ! Field
        do row = 1, FH
            call lb_clear()
            call lb_add('|')

            do col = 1, FW
                ! Ball
                if (row == iby .and. col == ibx) then
                    call lb_add('O')
                    cycle
                end if

                ! Bricks
                if (row >= 2 .and. row <= BROWS + 1) then
                    br = row - 1
                    bc = (col - 1) / BW + 1
                    if (bc >= 1 .and. bc <= BCOLS) then
                        if (bricks(br, bc) == 1) then
                            if (mod(col - 1, BW) == 0) then
                                call lb_add('[')
                            else if (mod(col, BW) == 0) then
                                call lb_add(']')
                            else
                                call lb_add('#')
                            end if
                            cycle
                        end if
                    end if
                end if

                ! Paddle
                if (row == FH - 1 .and. col >= px .and.             &
                    col <= px + pw_len - 1) then
                    call lb_add('=')
                    cycle
                end if

                ! Empty
                call lb_add(' ')
            end do

            call lb_add('|')
            call lb_out()
        end do

        ! Bottom border
        call lb_clear()
        call lb_add('+')
        call lb_add(repeat('-', FW))
        call lb_add('+')
        call lb_out()

        ! Controls
        write(*, '(A)') ' [A/D or Arrows] Move | [Q] Quit               '
    end subroutine

    ! --- End screen ---
    subroutine end_screen(keep_running)
        logical, intent(out) :: keep_running
        integer :: k, bonus
        character(len=12) :: stmp

        write(*, '(A)') ''
        if (victory) then
            bonus = lives * 50
            score = score + bonus
            write(*, '(A)') '  +==========================================+'
            write(*, '(A)') '  |    *** CONGRATULATIONS! YOU WON! ***     |'
            write(*, '(A)') '  +==========================================+'
            write(stmp, '(I0)') bonus
            write(*, '(A)') '  |  Victory Life Bonus: +' // trim(stmp) // ' pts'
            write(*, '(A,I0)') '  |  Final Score: ', score
            write(*, '(A)') '  +==========================================+'
        else
            write(*, '(A)') '  +==========================================+'
            write(*, '(A)') '  |             === GAME OVER ===            |'
            write(*, '(A)') '  +==========================================+'
            write(*, '(A,I0)') '  |  Final Score: ', score
            write(*, '(A)') '  +==========================================+'
        end if

        write(*, '(A)') ''
        write(*, '(A)') '  Press [R] to Play Again  or  [Q] to Quit'
        write(*, '(A)') ''

        do
            k = c_getch()
            if (k == 0 .or. k == 224) then
                k = c_getch()
                cycle
            end if

            if (k == ichar('r') .or. k == ichar('R')) then
                keep_running = .true.
                exit
            else if (k == ichar('q') .or. k == ichar('Q') .or. k == 27) then
                keep_running = .false.
                exit
            end if
        end do
    end subroutine

end program breakout_game
