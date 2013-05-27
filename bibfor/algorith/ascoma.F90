subroutine ascoma(meelem, numedd, solveu, lischa, matass)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/asmatr.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jedupo.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/nmchex.h'
    include 'asterfort/reajre.h'
    include 'asterfort/wkvect.h'
    character(len=19) :: solveu
    character(len=19) :: meelem(*)
    character(len=19) :: matass, lischa
    character(len=24) :: numedd
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL - UTILITAIRE)
!
! ASSEMBLAGE DE LA MATRICE DE RIGIDITE ASSOCIEE AUX CHARGEMENTS
! SUIVEURS
!
! ----------------------------------------------------------------------
!
!
! IN  MEELEM : LISTE DES MATR_ELEM
! IN  NUMEDD : NOM DE LA NUMEROTATION MECANIQUE
! IN  LISCHA : SD L_CHARGE
! IN  SOLVEU : NOM DU SOLVEUR DE NEWTON
! OUT MATASS : MATRICE GLOBALE ASSEMBLEE
!
!
!
!
    integer :: nbchme, jmec, iret
    integer :: k, jcoef, jlicoe
    character(len=24) :: licoef
    character(len=8) :: k8bid
    character(len=19) :: mesuiv
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    call nmchex(meelem, 'MEELEM', 'MESUIV', mesuiv)
    licoef = mesuiv(1:15)//'.COEF'
!
! --- NOMBRE DE CHARGEMENTS SUIVEURS
!
    call jeexin(licoef, iret)
    if (iret .eq. 0) then
        goto 999
    else
        call jelira(licoef, 'LONUTI', nbchme, k8bid)
        call jeveuo(mesuiv(1:19)//'.RELR', 'L', jmec)
        call jeveuo(licoef, 'L', jlicoe)
    endif
!
! --- AJOUT DES MESUIV
!
    call jedupo(mesuiv(1:19)//'.RERR', 'V', '&&ASCOMA           .RERR', .true.)
    call wkvect('&&ASCOMA.LISTE_COEF', 'V V R', 1, jcoef)
    do 777 k = 1, nbchme
        call jedetr('&&ASCOMA           .RELR')
        call reajre('&&ASCOMA', zk24(jmec+k-1), 'V')
        zr(jcoef) = zr(jlicoe+k-1)
        call asmatr(1, '&&ASCOMA           ', '&&ASCOMA.LISTE_COEF', numedd, solveu,&
                    lischa, 'CUMU', 'V', 1, matass)
777  end do
!
! --- MENAGE
!
    call jedetr('&&ASCOMA           .RELR')
    call jedetr('&&ASCOMA           .RERR')
    call jedetr('&&ASCOMA.LISTE_COEF')
!
999  continue
!
    call jedema()
end subroutine
