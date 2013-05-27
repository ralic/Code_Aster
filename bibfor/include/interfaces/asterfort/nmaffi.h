        interface
          subroutine nmaffi(fonact,sdconv,sdimpr,sderro,sddisc,nombcl)
            integer :: fonact(*)
            character(len=24) :: sdconv
            character(len=24) :: sdimpr
            character(len=24) :: sderro
            character(len=19) :: sddisc
            character(len=4) :: nombcl
          end subroutine nmaffi
        end interface
