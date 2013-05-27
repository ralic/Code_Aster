        interface
          subroutine mecalc(option,modele,chdepl,chgeom,chmate,chcara,&
     &chtemp,chtref,chtime,chnumc,chharm,chsig,cheps,chfreq,chmass,&
     &chmeta,charge,typcoe,alpha,calpha,chdynr,suropt,chelem,chelex,&
     &ligrel,base,ch1,ch2,chvari,compor,chtese,chdese,nopase,typese,&
     &chacse,codret)
            character(*) :: option
            character(*) :: modele
            character(*) :: chdepl
            character(*) :: chgeom
            character(*) :: chmate
            character(*) :: chcara(*)
            character(*) :: chtemp
            character(*) :: chtref
            character(*) :: chtime
            character(*) :: chnumc
            character(*) :: chharm
            character(*) :: chsig
            character(*) :: cheps
            character(*) :: chfreq
            character(*) :: chmass
            character(*) :: chmeta
            character(*) :: charge
            character(*) :: typcoe
            real(kind=8) :: alpha
            complex(kind=8) :: calpha
            character(*) :: chdynr
            character(*) :: suropt
            character(*) :: chelem
            character(*) :: chelex
            character(*) :: ligrel
            character(*) :: base
            character(*) :: ch1
            character(*) :: ch2
            character(*) :: chvari
            character(*) :: compor
            character(*) :: chtese
            character(*) :: chdese
            character(*) :: nopase
            integer :: typese
            character(*) :: chacse
            integer :: codret
          end subroutine mecalc
        end interface
