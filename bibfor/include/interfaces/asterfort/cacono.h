        interface
          subroutine cacono(noma,ndim,llist1,llist2,no1,no2,norm1,&
     &norm2,inoma)
            character(len=8) :: noma
            integer :: ndim
            character(len=24) :: llist1
            character(len=24) :: llist2
            integer :: no1
            integer :: no2
            real(kind=8) :: norm1(*)
            real(kind=8) :: norm2(*)
            integer :: inoma
          end subroutine cacono
        end interface
