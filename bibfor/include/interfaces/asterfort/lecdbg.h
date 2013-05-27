        interface
          subroutine lecdbg(ifl,icl,iv,rv,cv,cnl,mcl,nbm,nbg,dim,nob,&
     &irteti)
            integer :: nbm
            integer :: ifl
            integer :: icl
            integer :: iv
            real(kind=8) :: rv
            character(*) :: cv
            character(len=14) :: cnl
            character(len=8) :: mcl(nbm)
            integer :: nbg
            integer :: dim(nbm)
            character(len=24) :: nob(50,nbm)
            integer :: irteti
          end subroutine lecdbg
        end interface
