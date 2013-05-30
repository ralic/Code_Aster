subroutine pteequ(prchno, basz, neq, gds, ncmp,&
                  corr2)
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
!
!     ARGUMENTS:
!     ----------
    include 'jeveux.h'
!
    include 'asterfort/exisdg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/nbec.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    character(len=19) :: prchno
    integer :: neq, gds, ncmp, corr2(ncmp)
    character(len=*) :: basz
! ----------------------------------------------------------------------
!     BUT : CREER L'OBJET .DEEQ D'UN PROF_CHNO DANS LE CAS OU IL N'Y A
!           QUE DES NOEUDS PHYSIQUES (MAILLAGE)
!     IN:
!     PRCHNO : NOM D'UN PROF_CHNO
!     NEQ    : NOMBRE D'EQUATIONS (OU DE DDL) DU PROF_CHNO
!     GDS    : NUMERO DE LA GRANDEUR SIMPLE ASSOCIEE AU CHAMP.
!     NCMP   : NOMBRE DE CMPS DE PRCHNO (LONGUEUR DE CORR2)
!     CORR2  : CORRESPONDANCE KCMP_CH -> KCMP_GD
!
!     OUT:
!     PRCHNO EST COMPLETE DE L'OBJET ".DEEQ" V(I) DIM=2*NEQ
!         (CET OBJET EST DETRUIT S'IL EXISTE DEJA).
!     V((IDDL-1)*2+1)--> SI LE NOEUD SUPPORT DE L'EQUA. IDDL EST PHYS.:
!                           +NUMERO DU NOEUD
!                        SI LE NOEUD SUPPORT DE L'EQUA. IDDL EST UN
!                        LAGRANGE DE BLOCAGE :
!                           +NUMERO DU NOEUD PHYS. BLOQUE
!                        SI LE NOEUD SUPPORT DE L'EQUA. IDDL EST UN
!                        LAGRANGE DE LIAISON :
!                            0
!     V((IDDL-1)*2+2)--> SI LE NOEUD SUPPORT DE L'EQUA. IDDL EST PHYS.:
!                           + NUM. DANS L'ORDRE DU CATAL. DES GRAND.
!                           DE LA CMP CORRESPONDANT A L'EQUATION IDDL.
!                        SI LE NOEUD SUPPORT DE L'EQUA. IDDL EST UN
!                        LAGRANGE DE BLOCAGE :
!                           - NUM. DANS L'ORDRE DU CATAL. DES GRAND.
!                           DE LA CMP CORRESPONDANT AU BLOCAGE.
!                        SI LE NOEUD SUPPORT DE L'EQUA. IDDL EST UN
!                        LAGRANGE DE LIAISON :
!                            0
! ----------------------------------------------------------------------
!
!     FONCTIONS EXTERNES:
!     -------------------
!
!     VARIABLES LOCALES:
!     ------------------
    integer :: jdeeq, ncmpmx, nec, jnueq, nbligr, n1, jprno, i2
    integer :: nbno, j, iddl, iadg, k, ieq
!
    character(len=1) :: k1bid, base
!
!
    call jemarq()
    base=basz(1:1)
!
    call jedetr(prchno(1:19)//'.DEEQ')
    call wkvect(prchno(1:19)//'.DEEQ', base//' V I', 2*neq, jdeeq)
!
    call jelira(jexnum('&CATA.GD.NOMCMP', gds), 'LONMAX', ncmpmx, k1bid)
    nec = nbec(gds)
    if (ncmpmx .eq. 0) call u2mess('F', 'ASSEMBLA_24')
    if (nec .eq. 0) call u2mess('F', 'ASSEMBLA_25')
!
    call jeveuo(prchno(1:19)//'.NUEQ', 'L', jnueq)
    call jelira(prchno(1:19)//'.PRNO', 'NMAXOC', nbligr, k1bid)
    if (nbligr .ne. 1) call u2mess('F', 'ASSEMBLA_33')
!
    call jelira(jexnum(prchno(1:19)//'.PRNO', 1), 'LONMAX', n1, k1bid)
    if (n1 .le. 0) call u2mess('F', 'ASSEMBLA_34')
    call jeveuo(jexnum(prchno(1:19)//'.PRNO', 1), 'L', jprno)
!
    nbno = n1/ (nec+2)
    do 1 j = 1, nbno
        iddl = zi(jprno-1+ (j-1)* (nec+2)+1) - 1
        iadg = jprno - 1 + (j-1)* (nec+2) + 3
        do 2 i2 = 1, ncmp
            k = corr2(i2)
            if (exisdg(zi(iadg),k)) then
                iddl = iddl + 1
                ieq = zi(jnueq-1+iddl)
                zi(jdeeq-1+2* (ieq-1)+1) = j
                zi(jdeeq-1+2* (ieq-1)+2) = k
            endif
 2      continue
 1  end do
!
    call jedema()
end subroutine
