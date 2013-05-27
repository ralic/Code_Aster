        interface
          subroutine projkn(mailla,x3dca,lnuma,licnx,numail,nbcnx,cxma&
     &,xyzma,normal,itria,iproj,excent)
            character(len=8) :: mailla
            real(kind=8) :: x3dca(*)
            character(len=19) :: lnuma
            character(len=19) :: licnx
            integer :: numail
            integer :: nbcnx
            integer :: cxma(*)
            real(kind=8) :: xyzma(3,*)
            real(kind=8) :: normal(*)
            integer :: itria
            integer :: iproj
            real(kind=8) :: excent
          end subroutine projkn
        end interface
