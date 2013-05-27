subroutine dbgcal(optioz, ifm, nbin, lpaiz, lchiz,&
                  nbout, lpaouz, lchouz)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    include 'asterfort/jelstc.h'
    include 'asterfort/u2mesi.h'
    include 'asterfort/utimsd.h'
    character(len=*) :: optioz
    integer :: ifm
    integer :: nbin, nbout
    character(len=*) :: lpaiz(nbin), lpaouz(nbout)
    character(len=*) :: lchiz(nbin), lchouz(nbout)
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE POUR CALCUL
!
! DEBUGAGE DES CHAMPS IN/OUT POUR CALCUL
!
! ----------------------------------------------------------------------
!
!
! IN  OPTION : OPTION CALCULEE
! IN  IFM    : UNITE LOGIQUE D'IMPRESSION
! IN  NBIN   : NOMBRE DE CHAMPS IN
! IN  NBOUT  : NOMBRE DE CHAMPS OUT
! IN  LPAIZ  : NOM DES TYPES DE CHAMP D'ENTREE
! IN  LCHIZ  : NOM DES CHAMPS D'ENTREE
! IN  LPAOUZ : NOM DES TYPES DE CHAMP DE SORTIE
! IN  LCHOUZ : NOM DES CHAMPS DE SORTIE
!
! ----------------------------------------------------------------------
!
    integer :: ich
    character(len=16) :: option
!
    character(len=8) :: k8bid
    integer :: nbval, nbobj
!
! ---------------------------------------------------------------------
!
    option = optioz
!
    write(ifm,*) '***** CALCUL DE L OPTION <',option,'>'
    write(ifm,*) ' ** NBRE CHAMPS IN : ',nbin
    write(ifm,*) ' ** NBRE CHAMPS OUT: ',nbout
!
    write(ifm,*) '***** <CHAMPS_IN>'
    do 100 ich = 1, nbin
        write(ifm,*) ' * CHAMP IN  <',ich,'>'
        write(ifm,*) ' * PARAMETRE <',lpaiz(ich),'>'
        write(ifm,*) ' * CHAMP     <',lchiz(ich),'>'
        if (lpaiz(ich)(1:1) .eq. ' ') then
            call u2mesi('A', 'PRECALCUL_60', 1, ich)
        endif
        if (lchiz(ich)(1:1) .eq. ' ') then
            call u2mesi('A', 'PRECALCUL_61', 1, ich)
        endif
!
        call jelstc(' ', lchiz(ich)(1:19), 1, 0, k8bid,&
                    nbval)
        nbobj = -nbval
        if (nbobj .eq. 0) then
            call jelstc(' ', lchiz(ich), 1, 0, k8bid,&
                        nbval)
            nbobj = -nbval
            if (nbobj .eq. 0) then
                write(ifm,*) ' * SD INTROUVABLE !'
            else
                write(ifm,*) ' * RESUME DE LA SD :'
                call utimsd(ifm, -1, .true., .true., lchiz(ich),&
                            1, ' ')
            endif
        else
            write(ifm,*) ' * RESUME DE LA SD :'
            call utimsd(ifm, -1, .true., .true., lchiz(ich)(1:19),&
                        1, ' ')
        endif
100  end do
!
    write(ifm,*) '***** <CHAMPS_OUT>'
    do 200 ich = 1, nbout
        write(ifm,*) ' * CHAMP OUT <',ich,'>'
        write(ifm,*) ' * PARAMETRE <',lpaouz(ich),'>'
        write(ifm,*) ' * CHAMP     <',lchouz(ich),'>'
        if (lpaouz(ich)(1:1) .eq. ' ') then
            call u2mesi('A', 'PRECALCUL_62', 1, ich)
        endif
        if (lchouz(ich)(1:1) .eq. ' ') then
            call u2mesi('A', 'PRECALCUL_63', 1, ich)
        endif
!
        call jelstc(' ', lchouz(ich)(1:19), 1, 0, k8bid,&
                    nbval)
        nbobj = -nbval
        if (nbobj .eq. 0) then
            call jelstc(' ', lchouz(ich), 1, 0, k8bid,&
                        nbval)
            nbobj = -nbval
            if (nbobj .eq. 0) then
                write(ifm,*) ' * SD INTROUVABLE !'
            else
                write(ifm,*) ' * RESUME DE LA SD :'
                call utimsd(ifm, -1, .true., .true., lchouz(ich),&
                            1, ' ')
            endif
        else
            write(ifm,*) ' * RESUME DE LA SD :'
            call utimsd(ifm, -1, .true., .true., lchouz(ich)(1:19),&
                        1, ' ')
        endif
200  end do
end subroutine
