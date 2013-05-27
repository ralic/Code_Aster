subroutine numeok(acces, ilu, rlu, listrz, listiz,&
                  precis, crit, epsi, astock)
    implicit  none
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    logical :: astock
    character(len=*) :: acces, listrz, listiz, crit
    integer :: ilu, precis
    real(kind=8) :: rlu, epsi
!----------------------------------------------------------------------
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
!----------------------------------------------------------------------
!
!      VERIFICATION : LA VALEUR DU NUMERO D'ORDRE, DE L'INSTANT OU DE
!                     LA FREQUENCE EST-ELLE CELLE ATTENDUE ?
!
! IN  : ACCES  : K10  : TYPE D'ACCES
! IN  : ILU    : I    : VALEUR ENTIERE LUE DANS LE DATASET
! IN  : RLU    : R    : VALEUR REELLE LUE DANS LE DATASET
! IN  : LISTRZ : K19  : NOM DE L'OBJET CONTENANT LA LISTE DES INSTANTS
!                       OU DES FREQUENCES A LIRE
! IN  : LISTIZ : K19  : NOM DE L'OBJET CONTENANT LA LISTE DES
!                       NUMEROS D'ORDRE A LIRE
! IN  : PRECIS : I    : INDICATEUR DE VERIFICATION DE LA PRECISION
! IN  : CRIT   : K8   : CRITERE (RELATIF OU ABSOLU)
! IN  : EPSI   : R    : PRECISION
! OUT : ASTOCK : L    : .TRUE.  ON STOCKE LES RESULTATS
!                       .FALSE. ON NE STOCKE PAS LES RESULTATS
!----------------------------------------------------------------------
!
    real(kind=8) :: tref
    integer :: jlist, nbordr, iord
    integer :: i
    character(len=19) :: listis, listr8
    character(len=8) :: k8b
!
    call jemarq()
!
    astock = .false.
    listis = listiz
    listr8 = listrz
!
!- TOUT_ORDRE
!
    if (acces .eq. 'TOUT_ORDRE') then
        astock = .true.
        goto 70
    endif
!
!- NUME_ORDRE
!
    if (acces .eq. 'NUME_ORDRE') then
        call jeveuo(listis//'.VALE', 'L', jlist)
        call jelira(listis//'.VALE', 'LONMAX', nbordr, k8b)
        do 10 i = 1, nbordr
            if (ilu .eq. zi(jlist-1+i)) then
                astock = .true.
                goto 70
            endif
10      continue
    endif
!
!- INST
!
    if (acces .eq. 'INST') then
        call jeveuo(listr8//'.VALE', 'L', jlist)
        call jelira(listr8//'.VALE', 'LONMAX', nbordr, k8b)
        do 20 i = 1, nbordr
            if (rlu .eq. zr(jlist)) then
                astock = .true.
                goto 70
            endif
20      continue
    endif
!
!- LIST_INST
!
    if (acces .eq. 'LIST_INST') then
        call jeveuo(listr8//'.VALE', 'L', jlist)
        call jelira(listr8//'.VALE', 'LONMAX', nbordr, k8b)
        do 30 i = 1, nbordr
            if (ilu .eq. zr(jlist)) then
                astock = .true.
                goto 70
            endif
30      continue
    endif
!
!- FREQ
!
    if (acces .eq. 'FREQ') then
        call jeveuo(listr8//'.VALE', 'L', jlist)
        call jelira(listr8//'.VALE', 'LONMAX', nbordr, k8b)
        do 40 i = 1, nbordr
            if (ilu .eq. zr(jlist)) then
                astock = .true.
                goto 70
            endif
40      continue
    endif
!
!- LIST_FREQ
!
    if (acces .eq. 'LIST_FREQ') then
        call jeveuo(listr8//'.VALE', 'L', jlist)
        call jelira(listr8//'.VALE', 'LONMAX', nbordr, k8b)
        do 50 i = 1, nbordr
            if (ilu .eq. zr(jlist)) then
                astock = .true.
                goto 70
            endif
50      continue
    endif
!
!- VERIFICATION DE LA PRECISION ET DU CRITERE ---
!
    if (precis .ne. 0) then
        if (acces .ne. 'TOUT_ORDRE' .and. acces .ne. 'NUME_ORDRE') then
            do 60 iord = 1, nbordr
                tref = zr(jlist+iord-1)
                if (crit(1:4) .eq. 'RELA') then
                    if (abs(tref-rlu) .le. abs(epsi*rlu)) then
                        astock = .true.
                        goto 70
                    endif
                else if (crit(1:4).eq.'ABSO') then
                    if (abs(tref-rlu) .le. abs(epsi)) then
                        astock = .true.
                        goto 70
                    endif
                endif
60          continue
        endif
    endif
!
70  continue
!
    call jedema()
!
end subroutine
