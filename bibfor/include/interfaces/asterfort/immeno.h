        interface
          subroutine immeno(ncncin,nmabet,mailla,x3dca,noebe,numail,&
     &nbcnx,cxma,xyzma,itetra,xbar,immer)
            character(len=24) :: ncncin
            character(len=24) :: nmabet
            character(len=8) :: mailla
            real(kind=8) :: x3dca(*)
            integer :: noebe
            integer :: numail
            integer :: nbcnx
            integer :: cxma(*)
            real(kind=8) :: xyzma(3,*)
            integer :: itetra
            real(kind=8) :: xbar(*)
            integer :: immer
          end subroutine immeno
        end interface
