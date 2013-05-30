subroutine pjspma(corres, cham1, cham2, prol0, ligre2,&
                  noca, base, iret)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!  COMMANDE:  PROJ_CHAMP
!  ROUTINE "CHAPEAU" : LA PROJECTION AUX SOUS-POINTS (FAMILLE MATER)
!                         *  *           *    *               **
!
!  ELLE EST CONSTITUEE DE DEUX TEMPS
!    APPEL A PJXXCH (USUEL POUR TOUS LES CHAMPS, CF. OP0166 OU PJXXPR)
!    APPEL A PJCOR2 (RETOUR AUX SOUS-POINTS)
!
!
    implicit   none
!
! 0.1. ==> ARGUMENTS
!
    include 'asterfort/cescel.h'
    include 'asterfort/cnocns.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/pjcor2.h'
    include 'asterfort/pjxxch.h'
    include 'asterfort/titre.h'
    character(len=1) :: base
    character(len=8) :: prol0, noca
    character(len=16) :: corres
    character(len=19) :: cham1
    character(len=19) :: cham2, ligre2
    integer :: iret
!
! 0.3. ==> VARIABLES LOCALES
!
!
    integer :: nncp, ierd, ibid
!
    character(len=4) :: tycha2
    character(len=8) :: nompar
    character(len=16) :: option
    character(len=19) :: chauxs
    character(len=19) :: prfchn
    character(len=19) :: cns1, ch2s
!
!
!
! DEB ------------------------------------------------------------------
    call jemarq()
    call infmaj()
    call titre()
!
!     PROJECTION DU CHAMP SUR LE MAILLAGE MASP
    chauxs = '&&PJSPMA'//'.CHAS'
    tycha2 = 'NOEU'
    call pjxxch(corres, cham1, chauxs, tycha2, ' ',&
                prol0, ligre2, base, iret)
    if (iret .ne. 0) goto 999
!
!     CHAUXS : VALEUR AUX NOEUD DU MAILLAGE MASP
!
!
!     -- ON TRANSFORME LE CHAM_NO PROJETE EN UN CHAM_NO_S
    cns1 = '&&PJSPMA'//'.CH1S'
    call cnocns(chauxs, 'G', cns1)
!
!     CNS1 : CHAM_NO_S DES VALEURS AUX NOEUDS DE MASP
!
!     ON TRANSPOSE LE CHAMP SUR LE MODELE 2 (ELGA SOUS-POINTS)
    ch2s = '&&OP0166'//'.CH2S'
    call pjcor2(noca, cns1, ch2s, ligre2, corres,&
                nompar, ierd)
!
    option = 'INI_SP_MATER'
!
    call cescel(ch2s, ligre2, option, nompar, prol0,&
                nncp, 'G', cham2, 'A', ierd)
!
    call dismoi('F', 'PROF_CHNO', chauxs, 'CHAM_NO', ibid,&
                prfchn, ibid)
    call detrsd('PROF_CHNO', prfchn)
!
!
    call detrsd('CHAM_NO_S', cns1)
    call detrsd('CHAM_ELEM_S', ch2s)
!
999  continue
    call detrsd('CHAM_NO', chauxs)
!
    call jedema()
end subroutine
