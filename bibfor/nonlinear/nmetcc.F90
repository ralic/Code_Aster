subroutine nmetcc(sdieto, compor, sddyna, sdpost, resoco,&
                  nbcham, zioch)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit     none
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/ndynkk.h'
    include 'asterfort/nmlesd.h'
    integer :: nbcham, zioch
    character(len=24) :: sdieto, compor
    character(len=19) :: sddyna, sdpost
    character(len=24) :: resoco
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (GESTION IN ET OUT)
!
! NOM DU CHAMP DANS OP0070
!
! ----------------------------------------------------------------------
!
! SI NOM = CHAP#TYPCHA# : CHAMP DANS VARIABLE CHAPEAU TYPCHA
!
! IN  MODELE : NOM DU MODELE
! IN  COMPOR : CARTE COMPORTEMENT
! IN  SDDYNA : SD DYNAMIQUE
! IN  SDPOST : SD POUR POST-TRAITEMENTS (CRIT_STAB ET MODE_VIBR)
! IN  RESOCO : SD DE RESOLUTION DU CONTACT
! IN  SDIETO : SD GESTION IN ET OUT
!
! ----------------------------------------------------------------------
!
    character(len=24) :: iolcha
    integer :: jiolch
    character(len=24) :: nomcha, nomchx
    character(len=19) :: xindco, xcohes, xseuco
    character(len=24) :: nochco
    integer :: jnochc
    character(len=19) :: cnoinr
    character(len=19) :: vecfla, vecvib, vecsta
    character(len=19) :: depabs, vitabs, accabs
    real(kind=8) :: r8bid
    integer :: ibid
    integer :: icham
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- NOM DES CHAMPS NON-STANDARDS (PAS DANS UNE VARIABLE CHAPEAU)
!
    xindco = resoco(1:14)//'.XFIN'
    xcohes = resoco(1:14)//'.XCOH'
    xseuco = resoco(1:14)//'.XFSE'
    call nmlesd('POST_TRAITEMENT', sdpost, 'SOLU_MODE_FLAM', ibid, r8bid,&
                vecfla)
    call nmlesd('POST_TRAITEMENT', sdpost, 'SOLU_MODE_STAB', ibid, r8bid,&
                vecsta)
    call nmlesd('POST_TRAITEMENT', sdpost, 'SOLU_MODE_VIBR', ibid, r8bid,&
                vecvib)
    call ndynkk(sddyna, 'DEPABS', depabs)
    call ndynkk(sddyna, 'VITABS', vitabs)
    call ndynkk(sddyna, 'ACCABS', accabs)
!
! --- ACCES SD CHAMPS
!
    iolcha = sdieto(1:19)//'.LCHA'
    call jeveuo(iolcha, 'E', jiolch)
!
! --- NOM DU CHAMP DANS OP0070
! --- SI CHAP#NOMCHA# : CHAMP DANS VARIABLE CHAPEAU NOMCHA
!
    do 40 icham = 1, nbcham
        nomcha = zk24(jiolch+zioch*(icham-1)+1-1)
        if (nomcha .eq. 'DEPL') then
            nomchx = 'CHAP#VALINC#DEPMOI'
        else if (nomcha.eq.'VITE') then
            nomchx = 'CHAP#VALINC#VITMOI'
        else if (nomcha.eq.'ACCE') then
            nomchx = 'CHAP#VALINC#ACCMOI'
        else if (nomcha.eq.'SIEF_ELGA') then
            nomchx = 'CHAP#VALINC#SIGMOI'
        else if (nomcha.eq.'VARI_ELGA') then
            nomchx = 'CHAP#VALINC#VARMOI'
        else if (nomcha.eq.'STRX_ELGA') then
            nomchx = 'CHAP#VALINC#STRMOI'
        else if (nomcha.eq.'COMPORTEMENT') then
            nomchx = compor
        else if (nomcha.eq.'VALE_CONT') then
            nochco = resoco(1:14)//'.NOCHCO'
            call jeveuo(nochco, 'L', jnochc)
            cnoinr = zk24(jnochc+2-1)(1:19)
            nomchx = cnoinr
        else if (nomcha.eq.'INDC_ELEM') then
            nomchx = xindco
        else if (nomcha.eq.'SECO_ELEM') then
            nomchx = xseuco
        else if (nomcha.eq.'COHE_ELEM') then
            nomchx = xcohes
        else if (nomcha.eq.'MODE_FLAMB') then
            nomchx = vecfla
        else if (nomcha.eq.'MODE_STAB') then
            nomchx = vecsta
        else if (nomcha.eq.'DEPL_VIBR') then
            nomchx = vecvib
        else if (nomcha.eq.'DEPL_ABSOLU') then
            nomchx = depabs
        else if (nomcha.eq.'VITE_ABSOLU') then
            nomchx = vitabs
        else if (nomcha.eq.'ACCE_ABSOLU') then
            nomchx = accabs
        else if (nomcha.eq.'FORC_NODA') then
            nomchx = 'CHAP#VEASSE#CNFINT'
        else if (nomcha.eq.'FORC_AMOR') then
            nomchx = 'CHAP#VALINC#FAMMOI'
        else if (nomcha.eq.'FORC_LIAI') then
            nomchx = 'CHAP#VALINC#FLIMOI'
        endif
        zk24(jiolch+zioch*(icham-1)+6-1) = nomchx
40  end do
!
    call jedema()
end subroutine
