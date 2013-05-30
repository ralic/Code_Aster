subroutine pjelga(nomo2, cham1, ligre1, prol0, corres,&
                  leres1, ligre2, iret)
! person_in_charge: jacques.pellet at edf.fr
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
!
!  COMMANDE:  PROJ_CHAMP
!  ROUTINE "CHAPEAU" CONCERNANT LA PROJECTION DE CHAM_ELEM (ELGA)
!                                  *  *                     ****
!
!  ELLE EST CONSTITUEE DE TROIS TEMPS
!    APPEL A ECLPGC (ECLATEMENT DU CHAMP)
!    APPEL A PJXXCH (USUEL POUR TOUS LES CHAMPS, CF. OP0166 OU PJXXPR)
!    APPEL A PJCORR (RETOUR AUX POINTS DE GAUSS)
!
!
    implicit   none
!
! 0.1. ==> ARGUMENTS
!
    include 'asterfort/celfpg.h'
    include 'asterfort/cescel.h'
    include 'asterfort/cnocns.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/eclpgc.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/pjcorr.h'
    include 'asterfort/pjxxch.h'
    include 'asterfort/titre.h'
    character(len=8) :: nomo2, prol0
    character(len=16) :: corres
    character(len=19) :: cham1, ligre1
    character(len=19) :: leres1, ligre2
    integer :: iret
!
! 0.3. ==> VARIABLES LOCALES
!
!
    integer :: nncp
    character(len=4) :: tycha2
    character(len=8) :: ma1p
    character(len=8) :: nompar
    character(len=16) :: option
    character(len=19) :: cham1e, chauxs, chbid
    character(len=19) :: prfchn
    character(len=19) :: cns1, ch2s
    character(len=24) :: nomfpg
!
    integer :: jcnsv, ibid
!
!
! DEB ------------------------------------------------------------------
    call jemarq()
    call infmaj()
    call titre()
!
    chbid=cham1
    nomfpg='&&OP0166.NOMFPG'
    call celfpg(cham1, nomfpg, iret)
!
!     -- ON RECUPERE LE NOM DE L'OPTION ET LE NOM DU PARAMETRE
!        POUR LE CHAM_ELEM VIVANT SUR LE MAILLAGE 1
    call dismoi('F', 'NOM_OPTION', cham1, 'CHAM_ELEM', ibid,&
                option, ibid)
    call dismoi('F', 'NOM_PARAM', cham1, 'CHAM_ELEM', ibid,&
                nompar, ibid)
!
!
!     -- ECLATEMENT DU CHAMP VIVANT SUR LE MAILLAGE 1
!        EN UN CHAMP VIVANT SUR LE MAILLAGE 1 PRIME (MAILLAGE ECLATE)
    ma1p='&&PJELC1'
    cham1e='&&PJELGA'//'.CHAM1E'
!
    prfchn = '&&PJELGA.PRFCHN'
    call eclpgc(cham1, cham1e, ligre1, ma1p, prfchn,&
                nomfpg)
!
    chauxs = '&&PJELGA'//'.CHAS'
    tycha2= ' '
    call pjxxch(corres, cham1e, chauxs, tycha2, ' ',&
                prol0, ' ', 'G', iret)
!
!     -- ON TRANSFORME LE CHAM_NO PROJETE EN UN CHAM_NO_S
    cns1 = '&&PJELGA'//'.CH1S'
    call cnocns(chauxs, 'G', cns1)
    call jeveuo(cns1//'.CNSV', 'L', jcnsv)
!
!
!     -- IL FAUT MAINTENANT REVENIR AUX CHAM_ELEM (ELGA)
    ch2s = '&&OP0166'//'.CH2S'
    call pjcorr(nomo2, chbid, cns1, ch2s, ligre2,&
                corres, option, nompar, iret)
!
    call cescel(ch2s, ligre2, option, nompar, prol0,&
                nncp, 'G', leres1, 'A', iret)
!
!
    call detrsd('MAILLAGE', ma1p)
    call detrsd('CHAM_NO_S', cns1)
!
    call dismoi('F', 'PROF_CHNO', cham1e, 'CHAM_NO', ibid,&
                prfchn, ibid)
    call detrsd('PROF_CHNO', prfchn)
    call detrsd('CHAM_NO', cham1e)
!
    call dismoi('F', 'PROF_CHNO', chauxs, 'CHAM_NO', ibid,&
                prfchn, ibid)
    call detrsd('PROF_CHNO', prfchn)
    call detrsd('CHAM_NO', chauxs)
!
    call jedema()
end subroutine
