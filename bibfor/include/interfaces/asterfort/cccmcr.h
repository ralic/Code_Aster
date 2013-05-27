        interface
          subroutine cccmcr(jcesdd,numma,jrepe,jconx2,jconx1,jcoord,&
     &adcar1,adcar2,ialpha,ibeta,iepais,jalpha,jbeta,jgamma,ligrmo,ino,&
     &pgl,modeli,codret)
            integer :: jcesdd
            integer :: numma
            integer :: jrepe
            integer :: jconx2
            integer :: jconx1
            integer :: jcoord
            integer :: adcar1(3)
            integer :: adcar2(3)
            integer :: ialpha
            integer :: ibeta
            integer :: iepais
            integer :: jalpha
            integer :: jbeta
            integer :: jgamma
            character(len=19) :: ligrmo
            integer :: ino
            real(kind=8) :: pgl(3,3)
            character(len=16) :: modeli
            integer :: codret
          end subroutine cccmcr
        end interface
