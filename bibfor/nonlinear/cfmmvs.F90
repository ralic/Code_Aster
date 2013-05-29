subroutine cfmmvs(resoco, npt, jeux, loca)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    include 'jeveux.h'
    include 'asterc/r8vide.h'
    include 'asterfort/cfmmvd.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    character(len=24) :: resoco
    character(len=24) :: jeux, loca
    integer :: npt
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE VERIF)
!
! REMPLISSAGE SD VALE_CONT
!
! ----------------------------------------------------------------------
!
!
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  JEUX   : NOM DE LA SD STOCKANT LE JEU
! IN  LOCA   : NUMERO DU NOEUD POUR LE POINT DE CONTACT (-1 SI LE POINT
!              N'EST PAS UN NOEUD ! )
! IN  NPT    : NOMBRE DE POINTS EN MODE VERIF
!
!
!
!
    character(len=24) :: nochco
    integer :: jnochc
    character(len=19) :: cnsinr
    integer :: jcnsvr, jcnslr
    integer :: jjeux, jloca
    integer :: ipt
    real(kind=8) :: jeu, varc
    integer :: numnoe
    integer :: zresu
    logical :: lsauv
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    zresu = cfmmvd('ZRESU')
!
! --- NOM DU CHAM_NO VALE_CONT
!
    nochco = resoco(1:14)//'.NOCHCO'
    call jeveuo(nochco, 'L', jnochc)
    cnsinr = zk24(jnochc+1-1)(1:19)
!
! --- ACCES AU CHAM_NO_S POUR LE CONTACT
!
    call jeveuo(cnsinr(1:19)//'.CNSV', 'E', jcnsvr)
    call jeveuo(cnsinr(1:19)//'.CNSL', 'E', jcnslr)
!
! --- ACCES SD PROVISOIRES
!
    call jeveuo(jeux, 'L', jjeux)
    call jeveuo(loca, 'L', jloca)
!
! --- REMPLISSAGE
!
    do 10 ipt = 1, npt
!
! ----- INFORMATIONS SUR LE POINT
!
        jeu = zr(jjeux+ipt-1)
        numnoe = zi(jloca+ipt-1)
        lsauv = .true.
!
! ----- ETAT DU CONTACT
!
        if (jeu .lt. 0.d0) then
            varc = 1.d0
        else
            varc = 0.d0
        endif
!
        if (numnoe .eq. -1) lsauv = .false.
        if (jeu .eq. r8vide()) lsauv = .false.
!
! ----- REMPLISSAGE EFFECTIF
!
        if (lsauv) then
            zr(jcnsvr-1+zresu*(numnoe-1)+1 ) = varc
            zr(jcnsvr-1+zresu*(numnoe-1)+2 ) = jeu
            zl(jcnslr-1+zresu*(numnoe-1)+1 ) = .true.
            zl(jcnslr-1+zresu*(numnoe-1)+2 ) = .true.
        endif
!
10  end do
!
    call jedema()
end subroutine
