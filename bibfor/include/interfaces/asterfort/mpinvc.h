        interface
          subroutine mpinvc(nbmesu,nbmode,nbabs,phi,cmesu,coef,xabs,&
     &lfonct,ceta,cetap,ceta2p)
            integer :: nbabs
            integer :: nbmode
            integer :: nbmesu
            real(kind=8) :: phi(nbmesu,nbmode)
            complex(kind=8) :: cmesu(nbmesu,nbabs)
            real(kind=8) :: coef(*)
            real(kind=8) :: xabs(nbabs)
            logical :: lfonct
            complex(kind=8) :: ceta(nbmode,nbabs)
            complex(kind=8) :: cetap(nbmode,nbabs)
            complex(kind=8) :: ceta2p(nbmode,nbabs)
          end subroutine mpinvc
        end interface
