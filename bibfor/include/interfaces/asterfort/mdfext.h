        interface
          subroutine mdfext(tinit,dt,neqgen,nbexci,idescf,nomfon,coefm&
     &,liad,inumor,nbpas,f)
            integer :: neqgen
            real(kind=8) :: tinit
            real(kind=8) :: dt
            integer :: nbexci
            integer :: idescf(*)
            character(len=8) :: nomfon(*)
            real(kind=8) :: coefm(*)
            integer :: liad(*)
            integer :: inumor(*)
            integer :: nbpas
            real(kind=8) :: f(neqgen,*)
          end subroutine mdfext
        end interface
