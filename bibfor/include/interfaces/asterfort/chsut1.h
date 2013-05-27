        interface
          subroutine chsut1(chs1,nomgd2,ncmp,lcmp1,lcmp2,base,chs2)
            integer :: ncmp
            character(*) :: chs1
            character(*) :: nomgd2
            character(len=8) :: lcmp1(ncmp)
            character(len=8) :: lcmp2(ncmp)
            character(*) :: base
            character(*) :: chs2
          end subroutine chsut1
        end interface
