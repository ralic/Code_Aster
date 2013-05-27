subroutine xtailm(ndim, vecdir, numa, typma, jcoor,&
                  jconx1, jconx2, ipt, jtail)
    implicit none
!
    include 'jeveux.h'
    include 'asterfort/conare.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'blas/ddot.h'
    integer :: ndim, numa, jcoor, jconx1, jconx2, ipt, jtail
    real(kind=8) :: vecdir(ndim)
    character(len=8) :: typma
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! person_in_charge: samuel.geniaut at edf.fr
!       ----------------------------------------------------------------
!       DETERMINATION DE LA TAILLE MAXIMALE DE LA MAILLE CONNECTEE AU
!       POINT DU FOND IPT SUIVANT LA DIRECTION DU VECTEUR VECDIR
!       ----------------------------------------------------------------
!    ENTREES
!       NDIM   : DIMENSION DU MODELE
!       VECDIR : VECTEUR TANGENT
!       NUMA   : NUMERO DE LA MAILLE COURANTE
!       TYPMA  : TYPE DE LA MAILLE COURANTE
!       JCOOR  : ADRESSE DU VECTEUR DES COORDONNEES DES NOEUDS
!       JCONX1 : ADRESSE DE LA CONNECTIVITE DU MAILLAGE
!       JCONX2 : LONGUEUR CUMULEE DE LA CONNECTIVITE DU MAILLAGE
!       IPT    : INDICE DU POINT DU FOND
!    SORTIE
!       JTAIL  : ADRESSE DU VECTEUR DES TAILLES MAXIMALES DES MAILLES
!                CONNECTEES AUX NOEUDS DU FOND DE FISSURE
!
!
    integer :: ar(12, 3)
    integer :: iar, ino1, ino2, k, nbar, nno1, nno2
    real(kind=8) :: arete(3), p
!     -----------------------------------------------------------------
!
    call jemarq()
!
    call conare(typma, ar, nbar)
!
!     BOUCLE SUR LE NOMBRE D'ARETES DE LA MAILLE NUMA
    do 10 iar = 1, nbar
!
        ino1 = ar(iar,1)
        nno1 = zi(jconx1-1 + zi(jconx2+numa-1) +ino1-1)
        ino2 = ar(iar,2)
        nno2 = zi(jconx1-1 + zi(jconx2+numa-1) +ino2-1)
!
!       VECTEUR REPRESENTANT L'ARETE IAR
        do 11 k = 1, ndim
            arete(k)=zr(jcoor-1+(nno1-1)*3+k)-zr(jcoor-1+(nno2-1)*3+k)
11      continue
!
!       PROJECTION DE L'ARETE IAR SUR LE VECTEUR TANGENT
        p = ddot(ndim,arete,1,vecdir,1)
        p = abs(p)
!
        if (p .gt. zr(jtail-1+ipt)) zr(jtail-1+ipt) = p
!
10  end do
!
    call jedema()
end subroutine
