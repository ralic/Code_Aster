        interface
          subroutine rbph02(mailla,numddl,chamno,nomgd,neq,nbnoeu,&
     &objve1,ncmp,objve2,objve3,objve4)
            character(len=8) :: mailla
            character(len=14) :: numddl
            character(*) :: chamno
            character(len=8) :: nomgd
            integer :: neq
            integer :: nbnoeu
            character(len=24) :: objve1
            integer :: ncmp
            character(len=24) :: objve2
            character(len=24) :: objve3
            character(len=24) :: objve4
          end subroutine rbph02
        end interface
