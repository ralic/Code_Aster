        interface
          subroutine lectit(ifl,icl,iv,rv,cv,cnl,mcl,nbm,nbg,dim,nbt,&
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
            integer :: nbt(nbm)
            integer :: irteti
          end subroutine lectit
        end interface
