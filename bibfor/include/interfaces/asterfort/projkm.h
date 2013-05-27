        interface
          subroutine projkm(nmabet,nbmabe,mailla,x3dca,noebe,lnuma,&
     &licnx,numail,nbcnx,cxma,xyzma,normal,itria,xbar,iproj,excent)
            character(len=24) :: nmabet
            integer :: nbmabe
            character(len=8) :: mailla
            real(kind=8) :: x3dca(*)
            integer :: noebe
            character(len=19) :: lnuma
            character(len=19) :: licnx
            integer :: numail
            integer :: nbcnx
            integer :: cxma(*)
            real(kind=8) :: xyzma(3,*)
            real(kind=8) :: normal(*)
            integer :: itria
            real(kind=8) :: xbar(*)
            integer :: iproj
            real(kind=8) :: excent
          end subroutine projkm
        end interface
