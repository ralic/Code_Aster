subroutine dismme(questi, nomobz, repi, repkz, ierd)
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
    implicit none
!     --     DISMOI(MATR_ELEM OU VECT_ELEM)
!     ARGUMENTS:
!     ----------
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/dismmo.h'
    include 'asterfort/dismre.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    integer :: repi, ierd
    character(len=*) :: questi
    character(len=*) :: nomobz, repkz
    character(len=32) :: repk
    character(len=19) :: nomob
! ----------------------------------------------------------------------
!    IN:
!       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
!       NOMOBZ : NOM D'UN OBJET DE CONCEPT MATR_ELEM
!    OUT:
!       REPI   : REPONSE ( SI ENTIERE )
!       REPKZ  : REPONSE ( SI CHAINE DE CARACTERES )
!       IERD   : CODE RETOUR (0--> OK, 1 --> PB)
!
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    character(len=7) :: typmat, kmpic, zero
    integer :: iret, i, i1, ialire, iarefe, nbresu, iexi
    character(len=8) :: mo, partit
    character(len=8) :: kbid
!
!
!
    call jemarq()
    repk = ' '
    repi = 0
    ierd = 0
!
    nomob = nomobz
    call jeveuo(nomob//'.RERR', 'L', iarefe)
    mo = zk24(iarefe-1+1)(1:8)
!
    if (questi .eq. 'NOM_MODELE') then
        repk = mo
!
    else if (questi.eq.'TYPE_MATRICE') then
        repk='SYMETRI'
        call jeexin(nomob//'.RELR', iret)
        if (iret .gt. 0) then
            call jelira(nomob//'.RELR', 'LONUTI', nbresu, kbid)
            if (nbresu .gt. 0) call jeveuo(nomob//'.RELR', 'L', ialire)
            do 1, i=1,nbresu
            call jeexin(zk24(ialire-1+i)(1:19)//'.NOLI', iexi)
            if (iexi .eq. 0) goto 1
            call dismre(questi, zk24(ialire-1+i), repi, typmat, i1)
            if ((i1.eq.0) .and. (typmat.eq.'NON_SYM')) then
                repk='NON_SYM'
                goto 9999
            endif
 1          continue
        endif
!
    else if (questi.eq.'ZERO') then
        repk='OUI'
        call jeexin(nomob//'.RELR', iret)
        if (iret .gt. 0) then
            call jelira(nomob//'.RELR', 'LONUTI', nbresu, kbid)
            if (nbresu .gt. 0) call jeveuo(nomob//'.RELR', 'L', ialire)
            do 4, i=1,nbresu
            call jeexin(zk24(ialire-1+i)(1:19)//'.NOLI', iexi)
            if (iexi .eq. 0) goto 4
            call dismre(questi, zk24(ialire-1+i), repi, zero, i1)
            if ((i1.eq.0) .and. (zero.eq.'NON')) then
                repk='NON'
                goto 9999
            endif
 4          continue
        endif
!
    else if (questi.eq.'PARTITION') then
        repk=' '
        call jeexin(nomob//'.RELR', iret)
        if (iret .gt. 0) then
            call jelira(nomob//'.RELR', 'LONUTI', nbresu, kbid)
            if (nbresu .gt. 0) call jeveuo(nomob//'.RELR', 'L', ialire)
            do 2, i=1,nbresu
            call jeexin(zk24(ialire-1+i)(1:19)//'.NOLI', iexi)
            if (iexi .eq. 0) goto 2
            call dismre(questi, zk24(ialire-1+i), repi, partit, i1)
            if (partit .ne. ' ' .and. repk .eq. ' ') repk=partit
            if (partit .ne. ' ') call assert(repk.eq.partit)
 2          continue
        endif
!
    else if (questi.eq.'MPI_COMPLET') then
        repk=' '
        call jeexin(nomob//'.RELR', iret)
        if (iret .gt. 0) then
            call jelira(nomob//'.RELR', 'LONUTI', nbresu, kbid)
            if (nbresu .gt. 0) call jeveuo(nomob//'.RELR', 'L', ialire)
            do 3, i=1,nbresu
            call jeexin(zk24(ialire-1+i)(1:19)//'.NOLI', iexi)
            if (iexi .eq. 0) goto 3
            call dismre(questi, zk24(ialire-1+i), repi, kmpic, i1)
            if (i .eq. 1) then
                repk=kmpic
            else
                call assert(repk.eq.kmpic)
            endif
 3          continue
        endif
!
    else if (questi.eq.'CHAM_MATER') then
        repk=zk24(iarefe-1+4)
!
    else if (questi.eq.'CARA_ELEM') then
        repk=zk24(iarefe-1+5)
!
    else if (questi.eq.'NOM_MAILLA') then
        call dismmo(questi, mo, repi, repk, ierd)
!
    else if (questi.eq.'PHENOMENE') then
        call dismmo(questi, mo, repi, repk, ierd)
!
    else if (questi.eq.'SUR_OPTION') then
        repk= zk24(iarefe-1+2)(1:16)
!
    else if (questi.eq.'NB_SS_ACTI') then
        if (zk24(iarefe-1+3) .eq. 'OUI_SOUS_STRUC') then
            call dismmo(questi, mo, repi, repk, ierd)
        else
            repi= 0
        endif
    else
        ierd=1
    endif
!
9999  continue
    repkz = repk
    call jedema()
end subroutine
