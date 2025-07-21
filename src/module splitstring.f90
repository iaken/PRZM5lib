module splitstring
    implicit none
contains
    function strsplitc(str) result(starray)
        ! Variables
        character(len=:), dimension(:), allocatable :: starray
        character(len=*), intent(in) :: str
        logical, dimension(:), allocatable :: iscomment
        integer :: n, c, i, offset = 0
        character, dimension(:), allocatable :: char1
        character(len=:), allocatable :: str2  ! a modified copy of str
        
        integer, allocatable :: lineends(:), linestarts(:), lend(:), lstart(:)
        integer :: nxt  ! the index after the newline
        

        ! get indices of newlines, and append the length of the string, if last character is not a newline:
        c = len_trim(str)
        if(str(c:c) == NEW_LINE('a') ) then 
            lineends = pack([(i,i=1,len(str))], transfer(str, 'a', len(str)) == NEW_LINE('a'))
        else
            ! The line ends will not be put into the final array, so put the last character 1 past string end
            lineends = [integer :: pack([(i,i=1,len(str))], transfer(str, 'a', len(str)) == NEW_LINE('a')), c]
        end if
        ! find the start of each line. This assumes the string does not start with a blank line
        n = size(lineends)
        linestarts = [1, lineends(1:n-1) + 1]
        ! find comment lines and remove them, storing the starts and ends in lstart and lend
        char1 =  [(str(linestarts(i):linestarts(i)), i=1,size(linestarts))]
        iscomment = char1 /= '*'
        lend = pack(lineends, iscomment)
        lstart = pack(linestarts, iscomment)
        ! copy str without LF or CR
        allocate(character(len=len(str)) :: str2)
        str2 = str
        do concurrent (i = 1:len(str))
            if(str(i:i) == achar(10) .or. str(i:i) == achar(13)) then
                str2(i:i) = ' '
            else
                str2(i:i) = str(i:i)
            end if
        end do
    
        starray = [ (str2(lstart(i):lend(i)), i=1, size(lend))] ! lend(i)-1 -> ignore newline character
    end function strsplitc

    ! this subroutine replaces lines after each read which increment the line counter, l
    ! and check the status of the read. 
    integer function afterread(l, status, message)
        implicit none
        integer, intent(in) :: l
        integer, intent(in) :: status
        character(len=*), intent(in) :: message
        afterread = l+1
        if(status /= 0) then
            print *, message, " line=",l
            stop
        end if
    end function
end module splitstring