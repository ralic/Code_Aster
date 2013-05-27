        interface
          subroutine stkcoo(ifl,icl,iv,rv,cv,cnl,mcl,nbm,num,coo,nno,&
     &irteti)
            integer :: nbm
            integer :: ifl
            integer :: icl
            integer :: iv
            real(kind=8) :: rv
            character(*) :: cv
            character(len=14) :: cnl
            character(len=8) :: mcl(nbm)
            integer :: num
            character(len=24) :: coo
            character(len=24) :: nno
            integer :: irteti
          end subroutine stkcoo
        end interface
