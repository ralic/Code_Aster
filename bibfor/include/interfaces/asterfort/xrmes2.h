        interface
          subroutine xrmes2(ndim,nbnase,cpt,in,ivois,jsigse,nno,nbcmp,&
     &jcnset,dsg11,dsg22,dsg12)
            integer :: nbnase
            integer :: ndim
            integer :: cpt
            integer :: in
            integer :: ivois
            integer :: jsigse
            integer :: nno
            integer :: nbcmp
            integer :: jcnset
            real(kind=8) :: dsg11(nbnase)
            real(kind=8) :: dsg22(nbnase)
            real(kind=8) :: dsg12(nbnase)
          end subroutine xrmes2
        end interface
