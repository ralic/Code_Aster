subroutine cordd2(jprn1, jprn2, ili, ecodl, nec,&
                  ncmp, n, nddloc, pos)
! aslint: disable=
    implicit none
!
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
! ----------------------------------------------------------------------
!     BUT:
!     ----
!     ROUTINE PARALLELE A CORDDL POUR LES SOUS-STRUCTURES.
!
!     IN
!     --
!     JPRN1,JPRN2 : ADRESSES DE PRNO ( OBJET ET POINTEUR DE LONGUEUR)
!     NEC  : NBEC(GD) (SUPPOSE = 1!)
!     ILI  : NUMERO DU LIGREL (ICI TOUJOURS 1).
!     N    : NUMERO GLOBAL DU NOEUD
!     ECODL(*) : ENTIER CODE DU NUMERO LOCAL DU NOEUD
!
!     OUT
!     ---
!     NDDLOC : NBRE DE DDL SUPPORTES PAR CE NOEUD SUR L ELEMENT
!     POS    : TABLEAU DE CORRESPONDANCE AVEC LES DDL SUR LE NOEUD
!              EN TANT QUE NOEUD GLOBAL
! ----------------------------------------------------------------------
#include "jeveux.h"
#include "asterfort/assert.h"
    integer :: nbecmx, ncmp
    parameter (nbecmx = 10)
    integer :: ifin(nbecmx)
    integer :: pos(1)
    integer :: ecodg, ecodl(*)
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
    integer :: ili, jprn1, jprn2, nec, iec, in
    integer :: nddloc, n, i, iecdg, iecdl
!
!     FONCTION D ACCES A PRNO
#define prno(ili,nunoel,l)   zi(jprn1-1+zi(jprn2+ili-1)+ (nunoel-1)* (nec+2)+l-1)
!
! - DEB ----------------------------------------------------------------
!
! --- IFIN DONNE POUR CHAQUE ENTIER CODE LE NOMBRE MAX DE DDLS
! --- QUE L'ON PEUT TROUVER SUR CET ENTIER :
!     ------------------------------------
    ASSERT(nec.gt.0.and.nec.le.nbecmx)
    ASSERT(ncmp.gt.0.and.ncmp.le.30*nbecmx)
    do 10 iec = 1, nec-1
        ifin(iec) = 30
10  end do
    ifin(nec) = ncmp - 30*(nec-1)
!
    in = 0
    nddloc = 0
!
! --- AJOUT DE LA BOUCLE 20 SUR LE NOMBRE D'ENTIERS CODES
! --- PAR RAPPORT A LA VERSION NORMALE DE CORDD2 , LES INSTRUCTIONS
! --- NE CHANGENT PAS, EXCEPTEE LA DEFINITION DE ECODL ET ECODG
! --- OU INTERVIENT L'INDICE D'ENTIER CODE :
!     ------------------------------------
    do 20 iec = 1, nec
!
!      ECODG = PRNO(ILI,N,3)
        ecodg = prno(ili,n,2+iec)
!
        do 30 i = 1, ifin(iec)
            ecodg = ecodg/2
            ecodl(iec) = ecodl(iec)/2
            iecdg = iand(1,ecodg)
            if (iecdg .gt. 0) then
                in = in + 1
                iecdl = iand(1,ecodl(iec))
                if (iecdl .gt. 0) then
                    nddloc = nddloc + 1
                    pos(nddloc) = in
                endif
            endif
30      continue
!
20  end do
!
end subroutine
