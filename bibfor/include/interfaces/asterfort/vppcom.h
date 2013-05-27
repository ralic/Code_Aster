        interface
          subroutine vppcom(lcomod,icom1,icom2,resui,resur,resuk,&
     &nbpari,nbparr,nbpark,mxresf,vectr,nconv,neq,typres)
            logical :: lcomod
            integer :: icom1
            integer :: icom2
            integer :: resui(*)
            real(kind=8) :: resur(*)
            character(*) :: resuk(*)
            integer :: nbpari
            integer :: nbparr
            integer :: nbpark
            integer :: mxresf
            real(kind=8) :: vectr(*)
            integer :: nconv
            integer :: neq
            character(len=16) :: typres
          end subroutine vppcom
        end interface
