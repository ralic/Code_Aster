        interface
          subroutine comatr(option,typev,nbproc,rang,vnconv,dim1i,&
     &dim2i,vecti,dim1r,dim2r,vectr,dim1c,dim2c,vectc)
            integer :: dim1c
            integer :: dim1r
            integer :: dim1i
            integer :: nbproc
            character(len=1) :: option
            character(len=1) :: typev
            integer :: rang
            integer :: vnconv(nbproc)
            integer :: dim2i
            integer :: vecti(dim1i,*)
            integer :: dim2r
            real(kind=8) :: vectr(dim1r,*)
            integer :: dim2c
            complex(kind=8) :: vectc(dim1c,*)
          end subroutine comatr
        end interface
