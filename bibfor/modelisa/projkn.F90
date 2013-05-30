subroutine projkn(mailla, x3dca, lnuma, licnx, numail,&
                  nbcnx, cxma, xyzma, normal, itria,&
                  iproj, excent)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!  DESCRIPTION : PROJECTION D'UN NOEUD CABLE SUR LE NOEUD BETON LE PLUS
!  -----------   PROCHE
!                APPELANT : PROJCA
!
!  IN     : MAILLA : CHARACTER*8 , SCALAIRE
!                    NOM DU CONCEPT MAILLAGE ASSOCIE A L'ETUDE
!  IN     : X3DCA  : REAL*8 , VECTEUR DE DIMENSION 3
!                    COORDONNEES DU NOEUD CABLE CONSIDERE
!  IN     : LNUMA  : CHARACTER*19 , SCALAIRE
!                    NOM D'UN VECTEUR D'ENTIERS CONTENANT LES NUMEROS
!                    DES MAILLES AUXQUELLES APPARTIENT LE NOEUD BETON
!                    LE PLUS PROCHE DU NOEUD CABLE
!  IN     : LICNX  : CHARACTER*19 , SCALAIRE
!                    NOM D'UN VECTEUR D'ENTIERS CONTENANT LES RANGS DU
!                    NOEUD BETON LE PLUS PROCHE DANS LES TABLES DE
!                    CONNECTIVITE DES MAILLES AUXQUELLES IL APPARTIENT
!  OUT    : NUMAIL : INTEGER , SCALAIRE
!                    NUMERO DE LA PREMIERE MAILLE A LAQUELLE APPARTIENT
!                    LE NOEUD BETON LE PLUS PROCHE
!  OUT    : NBCNX  : INTEGER , SCALAIRE
!                    NOMBRE DE NOEUDS DE LA PREMIERE MAILLE A LAQUELLE
!                    APPARTIENT LE NOEUD BETON LE PLUS PROCHE
!  OUT    : CXMA   : INTEGER , VECTEUR DE DIMENSION AU PLUS NNOMAX
!                    NUMEROS DES NOEUDS DE LA PREMIERE MAILLE A LAQUELLE
!                    APPARTIENT LE NOEUD BETON LE PLUS PROCHE
!                    (TABLE DE CONNECTIVITE)
!  OUT    : XYZMA  : REAL*8 , TABLEAU DE DIMENSIONS (3,NNOMAX)
!                    TABLEAU DES COORDONNEES DES NOEUDS DE LA PREMIERE
!                    MAILLE A LAQUELLE APPARTIENT LE NOEUD BETON LE
!                    PLUS PROCHE
!  OUT    : NORMAL : REAL*8 , VECTEUR DE DIMENSION 3
!                    COORDONNEES DANS LE REPERE GLOBAL DU VECTEUR
!                    ---------->          ---------->
!                    NOEBE,NOECA / DNRM2(NOEBE,NOECA)
!  OUT    : ITRIA  : INTEGER , SCALAIRE
!                    INDICATEUR DU SOUS-DOMAINE AUQUEL APPARTIENT LE
!                    NOEUD BETON LE PLUS PROCHE :
!                    ITRIA = 1 : TRIANGLE 1-2-3
!                    ITRIA = 2 : TRIANGLE 3-4-1
!  OUT    : IPROJ  : INTEGER , SCALAIRE
!                    INDICE DE PROJECTION
!                    IPROJ =  2  CAR LE POINT PROJETE COINCIDE AVEC LE
!                                NOEUD BETON LE PLUS PROCHE
!  OUT    : EXCENT : REAL*8 , SCALAIRE
!                    DISTANCE DU NOEUD CABLE AU NOEUD BETON LE PLUS
!                    PROCHE
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
!
! ARGUMENTS
! ---------
    include 'jeveux.h'
!
    include 'asterc/matfpe.h'
    include 'asterc/r8prem.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/r8inir.h'
    include 'blas/dnrm2.h'
    include 'blas/dscal.h'
    character(len=8) :: mailla
    character(len=19) :: lnuma, licnx
    integer :: numail, nbcnx, cxma(*), itria, iproj
    real(kind=8) :: x3dca(*), xyzma(3, *), normal(*), excent
!
! VARIABLES LOCALES
! -----------------
    integer :: icnx, inoma, jcoor, jcxma, jlicnx, jlnuma, noe
    real(kind=8) :: epsg, nrm2
    character(len=1) :: k1b
    character(len=24) :: conxma, coorno
!
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    call matfpe(-1)
!
    call jemarq()
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 1   ACCES AUX OBJETS UTILES - INITIALISATIONS
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    conxma = mailla//'.CONNEX'
    coorno = mailla//'.COORDO    .VALE'
    call jeveuo(coorno, 'L', jcoor)
!
    call jeveuo(lnuma, 'L', jlnuma)
    call jeveuo(licnx, 'L', jlicnx)
!
    epsg = 1.0d+08 * r8prem()
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 2   PROJECTION DU NOEUD CABLE SUR LE NOEUD BETON LE PLUS PROCHE
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    numail = zi(jlnuma)
    icnx = zi(jlicnx)
!
! 2.1 RECUPERATION DES INFORMATIONS CARACTERISANT LA TOPOLOGIE
! --- DE LA PREMIERE MAILLE A LAQUELLE APPARTIENT
!     LE NOEUD BETON LE PLUS PROCHE
!
    call jelira(jexnum(conxma, numail), 'LONMAX', nbcnx, k1b)
    call jeveuo(jexnum(conxma, numail), 'L', jcxma)
!.... AFFECTATION DE ITRIA
    if ((nbcnx.eq.3) .or. (nbcnx.eq.6)) then
        itria = 1
    else
        if ((icnx.eq.4) .or. (icnx.eq.7) .or. (icnx.eq.8)) then
            itria = 2
        else
            itria = 1
        endif
    endif
!
! 2.2 RECUPERATION DES NUMEROS ET DES COORDONNEES DES NOEUDS
! --- DE LA PREMIERE MAILLE A LAQUELLE APPARTIENT
!     LE NOEUD BETON LE PLUS PROCHE
!
    do 10 inoma = 1, nbcnx
        noe = zi(jcxma+inoma-1)
        cxma(inoma) = noe
        xyzma(1,inoma) = zr(jcoor+3*(noe-1) )
        xyzma(2,inoma) = zr(jcoor+3*(noe-1)+1)
        xyzma(3,inoma) = zr(jcoor+3*(noe-1)+2)
10  end do
!
! 2.3 PROJECTION DU NOEUD CABLE SUR LE NOEUD BETON LE PLUS PROCHE
! ---
    normal(1) = x3dca(1) - xyzma(1,icnx)
    normal(2) = x3dca(2) - xyzma(2,icnx)
    normal(3) = x3dca(3) - xyzma(3,icnx)
    excent = dnrm2(3,normal(1),1)
    nrm2 = dble(max(dnrm2(3,x3dca(1),1),dnrm2(3,xyzma(1,icnx),1)))
    if (nrm2 .eq. 0.0d0) nrm2 = 1.0d0
    if (dble(abs(excent))/nrm2 .lt. epsg) then
        excent = 0.0d0
        call r8inir(3, 0.0d0, normal(1), 1)
    else
        call dscal(3, 1.0d0/excent, normal(1), 1)
    endif
!.... AFFECTATION DE IPROJ
    iproj = 2
!
    call jedema()
!
! --- FIN DE PROJKN.
    call matfpe(1)
!
end subroutine
