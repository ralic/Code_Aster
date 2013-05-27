        interface
          subroutine mpinvr(nbmesu,nbmode,nbabs,phi,rmesu,coef,xabs,&
     &lfonct,reta)
            integer :: nbabs
            integer :: nbmode
            integer :: nbmesu
            real(kind=8) :: phi(nbmesu,nbmode)
            real(kind=8) :: rmesu(nbmesu,nbabs)
            real(kind=8) :: coef(*)
            real(kind=8) :: xabs(nbabs)
            logical :: lfonct
            real(kind=8) :: reta(nbmode,nbabs)
          end subroutine mpinvr
        end interface
