subroutine xcfacf(ptint, ptmax, ipt, ainter, lsn,&
                  lst, igeom, nno, ndim, typma,&
                  noma, nmaabs)
    implicit none
!
    include 'jeveux.h'
    include 'asterc/r8maem.h'
    include 'asterc/r8prem.h'
    include 'asterfort/confac.h'
    include 'asterfort/elref1.h'
    include 'asterfort/intfac.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/padist.h'
    include 'asterfort/xajpin.h'
    integer :: ptmax, ipt, igeom, nno, ndim, nmaabs
    real(kind=8) :: lsn(nno), lst(nno), ptint(*), ainter(*)
    character(len=8) :: typma, noma
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
! person_in_charge: samuel.geniaut at edf.fr
!              TROUVER LES PTS D'INTERSECTION ENTRE LE FOND DE FISSURE
!                 ET LES FACES POUR LES ELEMENTS EN FOND DE FISSURE
!
!     ENTREE
!       PTINT    : COORDONNEES DES POINTS D'INTERSECTION
!       PTMAX    : NOMBRE MAX DE POINTS D'INTERSECTION
!       IPT      : COMPTEUR DE NOMBRE DE POINTS D'INTERSECTION
!       AINTER   : INFOS SUR LES ARETES ASSOCIEES
!       LSN      : VALEURS DE LA LEVEL SET NORMALE
!       LST      : VALEURS DE LA LEVEL SET TANGENTE
!       IGEOM    : ADRESSE DES COORDONNEES DES NOEUDS DE L'ELT PARENT
!       NNO      : NOMBRE DE NOEUDS DE L'ELEMENT
!       NDIM     : DIMENSION DE L'ESPACE
!       TYPMA    : TYPE DE LA MAILLE ASSOCIEE A L'ELEMENT
!       NOMA     : NOM DU MAILLAGE
!       NMAABS   : INDICE DE LA MAILLE
!
!     SORTIE
!       PTINT    : COORDONNEES DES POINTS D'INTERSECTION
!       IPT      : COMPTEUR DE NOMBRE DE POINTS D'INTERSECTION
!       AINTER   : INFOS SUR LES ARETES ASSOCIEES
!
!     ------------------------------------------------------------------
!
    character(len=8) :: elref
    real(kind=8) :: rbid, maxlsn, minlsn, maxlst, minlst
    real(kind=8) :: a(3), b(3), c(3)
    real(kind=8) :: loncar, dst
    real(kind=8) :: m(3)
    integer :: i, nbf, ibid, ifq, j, codret
    integer :: fa(6, 4), ibid3(12, 3), indptf(3)
! ----------------------------------------------------------------------
!
    call jemarq()
    call elref1(elref)
!
!     INITIALISATION DES MIN ET MAX
    maxlsn=-1.d0*r8maem()
    minlsn=r8maem()
    maxlst=-1.d0*r8maem()
    minlst=r8maem()
!
!     RECHERCHE DU MIN ET MAX DES LEVEL SETS SUR LES NOEUDS
    do 100 i = 1, nno
        maxlsn=max(lsn(i),maxlsn)
        maxlst=max(lst(i),maxlst)
        minlsn=min(lsn(i),minlsn)
        minlst=min(lst(i),minlst)
100  end do
!
!     SI CE N'EST PAS UN ELEMENT EN FOND DE FISSURE, ON SORT
!     EN FAIT, CE TEST NE PERMET PAS DE DETECTER TOUS LES CAS
!     IL SE PEUT DONC QUE L'ON NE SORTE PAS MAIS QUE L'ON NE
!     SOIT PAS SUR UNE ELEMENT EN FOND DE FISSURE
    if (minlsn*maxlsn .ge. 0.d0 .or. minlst*maxlst .ge. 0.d0) goto 9999
!
    call confac(typma, ibid3, ibid, fa, nbf)
!
!     BOUCLE SUR LES FACES
    do 200 ifq = 1, nbf
!
!       RECHERCHE DES INTERSECTION ENTRE LE FOND DE FISSURE ET LA FACE
        call intfac(noma, nmaabs, ifq, fa, nno,&
                    lst, lsn, ndim, 'NON', ibid,&
                    ibid, igeom, m, indptf, rbid,&
                    rbid, codret)
        if (codret .eq. 0) goto 200
!
!       POUR IGNORER LES POINTS CONFONDUS AVEC CEUX
!       DETECTES DANS XCFACE LORSQUE LE PT EST EXACT SUR UNE ARETE
        do 250 j = 1, ipt
            dst=padist(ndim,m,ptint(ndim*(j-1)+1))
            if (dst .le. r8prem()) goto 200
250      continue
!
!       LONGUEUR CARACTERISTIQUE
        do 260 i = 1, ndim
            a(i) = zr(igeom-1+ndim*(fa(ifq,1)-1)+i)
            b(i) = zr(igeom-1+ndim*(fa(ifq,2)-1)+i)
            c(i) = zr(igeom-1+ndim*(fa(ifq,3)-1)+i)
260      continue
        loncar=(padist(ndim,a,b)+padist(ndim,a,c))/2.d0
!
!       ON AJOUTE A LA LISTE LE POINT M
!
        call xajpin(ndim, ptint, ptmax, ipt, ibid,&
                    m, loncar, ainter, 0, 0,&
                    0.d0)
!
200  end do
!
9999  continue
    call jedema()
end subroutine
