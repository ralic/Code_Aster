        interface
          subroutine vpstor(ineg,type,modes,nbmode,neq,vecpr8,vecpc8,&
     &mxresf,nbpari,nbparr,nbpark,nopara,mod45,resufi,resufr,resufk,&
     &iprec)
            integer :: mxresf
            integer :: neq
            integer :: ineg
            character(*) :: type
            character(*) :: modes
            integer :: nbmode
            real(kind=8) :: vecpr8(neq,*)
            complex(kind=8) :: vecpc8(neq,*)
            integer :: nbpari
            integer :: nbparr
            integer :: nbpark
            character(*) :: nopara(*)
            character(len=4) :: mod45
            integer :: resufi(mxresf,*)
            real(kind=8) :: resufr(mxresf,*)
            character(*) :: resufk(mxresf,*)
            integer :: iprec
          end subroutine vpstor
        end interface
