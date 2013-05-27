        interface
          subroutine nmedco(compor,option,imate,npg,lgpg,s,q,vim,vip,&
     &alphap,dalfs)
            integer :: lgpg
            integer :: npg
            character(len=16) :: compor(*)
            character(len=16) :: option
            integer :: imate
            real(kind=8) :: s(2)
            real(kind=8) :: q(2,2)
            real(kind=8) :: vim(lgpg,npg)
            real(kind=8) :: vip(lgpg,npg)
            real(kind=8) :: alphap(2)
            real(kind=8) :: dalfs(2,2)
          end subroutine nmedco
        end interface
