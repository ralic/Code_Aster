        interface
          subroutine postsm(option,fm,df,sigm,sigp,dsidep)
            character(len=16) :: option
            real(kind=8) :: fm(3,3)
            real(kind=8) :: df(3,3)
            real(kind=8) :: sigm(6)
            real(kind=8) :: sigp(6)
            real(kind=8) :: dsidep(6,3,3)
          end subroutine postsm
        end interface
