        interface
          subroutine vppfac(lmasse,masgen,vect,neq,nbvect,mxvect,&
     &masmod,facpar)
            integer :: mxvect
            integer :: neq
            integer :: lmasse
            real(kind=8) :: masgen(*)
            real(kind=8) :: vect(neq,*)
            integer :: nbvect
            real(kind=8) :: masmod(mxvect,*)
            real(kind=8) :: facpar(mxvect,*)
          end subroutine vppfac
        end interface
