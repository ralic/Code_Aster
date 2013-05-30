subroutine xffcr(nfon, jfono, jbaso, jtailo, jindpt,&
                 typfon, jfon, jbas, jtail)
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
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/padist.h'
    integer :: nfon, jfono, jbaso, jtailo, jindpt, jfon, jbas, jtail
    character(len=19) :: typfon
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM
!
!              ORDONNANCEMENT DES VECTEURS BASEFOND, FONDFISS ET
!              FOND.TAILLE_R
!
! ----------------------------------------------------------------------
!
!
! IN  NFON  :  NOMBRE DE POINTS AU FOND DE FISSURE
!     JFONO :  ADRESSE DES POINTS DU FOND DE FISSURE DÉSORDONNÉS
!     JBASO :  ADRESSE DES DIRECTIONS DE PROPAGATION DÉSORDONNÉES
!     JTAILO:  ADRESSE DES TAILLES MAXIMALES DE MAILLES DÉSORDONNÉES
!     JINDPT:  ADRESSE DES INDICES DES POINTS ORDONNES
!     TYPFON:  TYPE DU FOND DE FISSURE (OUVERT OU FERME)
!
! OUT JFON  :  ADRESSE DES POINTS DU FOND DE FISSURE ORDONNÉS
!     JBAS  :  ADRESSE DES DIRECTIONS DE PROPAGATION ORDONNÉES
!     JTAIL :  ADRESSE DES TAILLES MAXIMALES DE MAILLES ORDONNÉES
!
!
    integer :: indipt, ipt, k
    real(kind=8) :: m(3), p(3)
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    do 10 ipt = 1, nfon
!
        indipt = zi(jindpt-1+ipt)
!
        do 11 k = 1, 3
!
            zr(jfon-1+4*(ipt-1)+k) = zr(jfono-1+11*(indipt-1)+k)
            zr(jbas-1+6*(ipt-1)+k) = zr(jbaso-1+6*(indipt-1)+k)
            zr(jbas-1+6*(ipt-1)+k+3) = zr(jbaso-1+6*(indipt-1)+3+k)
!
11      continue
!
        zr(jfon-1+4*(ipt-1)+4) = zr(jfono-1+11*(indipt-1)+4)
        zr(jtail-1+ipt) = zr(jtailo-1+indipt)
!
10  end do
!
!     CAS D'UN FOND FERME: PREMIER POINT DU FOND = DERNIER POINT
    if (typfon .eq. 'FERME') then
!
        nfon = nfon + 1
!
        do 20 k = 1, 3
!
            zr(jfon-1+4*(nfon-1)+k) = zr(jfon-1+4*(1-1)+k)
            zr(jbas-1+6*(nfon-1)+k) = zr(jbas-1+6*(1-1)+k)
            zr(jbas-1+6*(nfon-1)+k+3) = zr(jbas-1+6*(1-1)+3+k)
!
            p(k) = zr(jfon-1+4*(nfon-1)+k)
            m(k) = zr(jfon-1+4*(nfon-2)+k)
!
20      continue
!
        zr(jfon-1+4*(nfon-1)+4)= zr(jfon-1+4*(nfon-2)+4) + padist(3,m,&
        p)
        zr(jtail-1+nfon) = zr(jtail-1+1)
!
    endif
!
    call jedema()
end subroutine
