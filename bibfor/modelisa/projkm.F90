subroutine projkm(nmabet, nbmabe, mailla, x3dca, noebe,&
                  lnuma, licnx, numail, nbcnx, cxma,&
                  xyzma, normal, itria, xbar, iproj,&
                  excent)
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
!  DESCRIPTION : TENTATIVE DE PROJECTION D'UN NOEUD CABLE SUR LES
!  -----------   MAILLES APPARTENANT A LA STRUCTURE BETON
!                APPELANT : PROJCA
!
!  IN     : NMABET : CHARACTER*24 ,
!                    OBJET CONTENANT LES MAILLES BETON
!  IN     : NBMABE : INTEGER , SCALAIRE
!                    NOMBRE DE MAILLE BETON
!  IN     : MAILLA : CHARACTER*8 , SCALAIRE
!                    NOM DU CONCEPT MAILLAGE ASSOCIE A L'ETUDE
!  IN     : X3DCA  : REAL*8 , VECTEUR DE DIMENSION 3
!                    COORDONNEES DU NOEUD CABLE CONSIDERE
!  IN     : NOEBE  : INTEGER , SCALAIRE
!                    NUMERO DU NOEUD BETON LE PLUS PROCHE DU NOEUD CABLE
!                    CONSIDERE
!  IN     : LNUMA  : CHARACTER*19 , SCALAIRE
!                    NOM D'UN VECTEUR D'ENTIERS POUR STOCKAGE DES
!                    NUMEROS DES MAILLES AUXQUELLES APPARTIENT LE
!                    NOEUD NOEBE
!                    DIMENSION REAJUSTEE EN SORTIE
!  IN     : LICNX  : CHARACTER*19 , SCALAIRE
!                    NOM D'UN VECTEUR D'ENTIERS POUR STOCKAGE DES RANGS
!                    DU NOEUD NOEBE DANS LES TABLES DE CONNECTIVITE DES
!                    MAILLES AUXQUELLES IL APPARTIENT
!                    DIMENSION REAJUSTEE EN SORTIE
!  OUT    : NUMAIL : INTEGER , SCALAIRE
!                    SI PROJECTION REUSSIE : NUMERO DE LA MAILLE SUR
!                    LAQUELLE EST REALISEE LA PROJECTION
!  OUT    : NBCNX  : INTEGER , SCALAIRE
!                    SI PROJECTION REUSSIE : NOMBRE DE NOEUDS DE LA
!                    MAILLE SUR LAQUELLE EST REALISEE LA PROJECTION
!  OUT    : CXMA   : INTEGER , VECTEUR DE DIMENSION AU PLUS NNOMAX
!                    SI PROJECTION REUSSIE : NUMEROS DES NOEUDS DE LA
!                    MAILLE SUR LAQUELLE EST REALISEE LA PROJECTION
!                    (TABLE DE CONNECTIVITE)
!  OUT    : XYZMA  : REAL*8 , TABLEAU DE DIMENSIONS (3,NNOMAX)
!                    SI PROJECTION REUSSIE : TABLEAU DES COORDONNEES
!                    DES NOEUDS DE LA MAILLE SUR LAQUELLE EST REALISEE
!                    LA PROJECTION
!  OUT    : NORMAL : REAL*8 , VECTEUR DE DIMENSION 3
!                    SI PROJECTION REUSSIE : COORDONNEES DANS LE REPERE
!                    GLOBAL DU VECTEUR NORMAL AU PLAN MOYEN DE LA MAILLE
!                    SUR LAQUELLE EST REALISEE LA PROJECTION
!  OUT    : ITRIA  : INTEGER , SCALAIRE
!                    SI PROJECTION REUSSIE : INDICATEUR DU SOUS-DOMAINE
!                    AUQUEL APPARTIENT LE POINT PROJETE :
!                    ITRIA = 1 : TRIANGLE 1-2-3
!                    ITRIA = 2 : TRIANGLE 3-4-1
!  OUT    : XBAR   : REAL*8 , VECTEUR DE DIMENSION 3
!                    SI PROJECTION REUSSIE : COORDONNEES BARYCENTRIQUES
!                    DU POINT PROJETE (BARYCENTRE DES SOMMETS DU
!                    TRIANGLE 1-2-3 OU 3-4-1)
!  OUT    : IPROJ  : INTEGER , SCALAIRE
!                    INDICE DE PROJECTION
!                    IPROJ = -1  PROJECTION NON REUSSIE
!                    IPROJ =  0  LE POINT PROJETE EST A L'INTERIEUR
!                                DE LA MAILLE
!                    IPROJ =  1X LE POINT PROJETE EST SUR UNE FRONTIERE
!                                DE LA MAILLE
!                    IPROJ =  2  LE POINT PROJETE COINCIDE AVEC UN DES
!                                NOEUDS DE LA MAILLE
!  OUT    : EXCENT : REAL*8 , SCALAIRE
!                    SI PROJECTION REUSSIE : EXCENTRICITE DU NOEUD
!                    CABLE PAR RAPPORT A LA MAILLE SUR LAQUELLE EST
!                    REALISEE LA PROJECTION
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
!
! ARGUMENTS
! ---------
    include 'jeveux.h'
!
    include 'asterc/r8prem.h'
    include 'asterfort/canorm.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jelibe.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexatr.h'
    include 'asterfort/projtq.h'
    include 'blas/daxpy.h'
    include 'blas/dcopy.h'
    include 'blas/dscal.h'
    character(len=32) :: jexnum
    character(len=8) :: mailla
    character(len=19) :: lnuma, licnx
    integer :: noebe, numail, nbcnx, cxma(*), itria, iproj, nbmabe
    real(kind=8) :: x3dca(*), xyzma(3, *), normal(*), xbar(*), excent
    character(len=24) :: nmabet
!
! VARIABLES LOCALES
! -----------------
    integer :: icnx, imail, inoma, jcoor, jcxma, jlicnx, jlnuma, jnumab, jtyma
    integer :: nbmaok, noe, ntyma, jconx1, jconx2
    real(kind=8) :: d, dmax, dx, dy, dz, epsg, x3dp(3)
    character(len=1) :: k1b
    character(len=24) :: conxma, coorno, tymama
!
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    call jemarq()
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 1   ACCES AUX OBJETS DU CONCEPT MAILLAGE
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    conxma = mailla//'.CONNEX'
    call jeveuo(conxma, 'L', jconx1)
    coorno = mailla//'.COORDO    .VALE'
    call jeveuo(coorno, 'L', jcoor)
    call jeveuo(nmabet, 'L', jnumab)
    tymama = mailla//'.TYPMAIL'
    call jeveuo(tymama, 'L', jtyma)
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 2   TENTATIVE DE PROJECTION DU NOEUD CABLE CONSIDERE SUR LES MAILLES
!     APPARTENANT A LA STRUCTURE BETON
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    epsg = 1.0d+08 * r8prem()
    nbmaok = 0
!
!.... BOUCLE SUR LES MAILLES APPARTENANT A LA STRUCTURE BETON, POUR
!.... RETROUVER LE NOEUD BETON LE PLUS PROCHE DANS LES CONNECTIVITES
!
    call jeveuo(jexatr(mailla//'.CONNEX', 'LONCUM'), 'L', jconx2)
!
    do 10 imail = 1, nbmabe
!
        numail = zi(jnumab+imail-1)
        nbcnx = zi(jconx2+numail)-zi(jconx2-1+numail)
!
        do 20 icnx = 1, nbcnx
!
!.......... SI LE NOEUD BETON EST RETROUVE DANS LES CONNECTIVITES,
!.......... TEST DE PROJECTION DU NOEUD CABLE SUR LA MAILLE COURANTE
!
            if (zi(jconx1-1+zi(jconx2+numail-1)+icnx-1) .eq. noebe) then
!
!............. ON NOTE LE NUMERO DE LA MAILLE ET L'INDICE DU NOEUD
!............. NOEBE DANS LA TABLE DE CONNECTIVITE ASSOCIEE
!
                nbmaok = nbmaok + 1
                call jeecra(lnuma, 'LONUTI', nbmaok, ' ')
                call jeveuo(lnuma, 'E', jlnuma)
                zi(jlnuma+nbmaok-1) = numail
                call jelibe(lnuma)
                call jeecra(licnx, 'LONUTI', nbmaok, ' ')
                call jeveuo(licnx, 'E', jlicnx)
                zi(jlicnx+nbmaok-1) = icnx
                call jelibe(licnx)
!
!............. RECUPERATION DES NUMEROS ET DES COORDONNEES DES NOEUDS
!............. DE LA MAILLE
!
                do 30 inoma = 1, nbcnx
                    noe = zi(jconx1-1+zi(jconx2+numail-1)+inoma-1)
                    cxma(inoma) = noe
                    xyzma(1,inoma) = zr(jcoor+3*(noe-1) )
                    xyzma(2,inoma) = zr(jcoor+3*(noe-1)+1)
                    xyzma(3,inoma) = zr(jcoor+3*(noe-1)+2)
30              continue
!
!............. RECUPERATION DE LA NORMALE AU PLAN DE LA MAILLE
!
                ntyma = zi(jtyma+numail-1)
                call canorm(xyzma(1, 1), normal(1), 3, ntyma, 1)
!
!............. EXCENTRICITE DU NOEUD DU CABLE ET COORDONNEES
!............. DU POINT PROJETE
!
                excent = normal(1)*(x3dca(1)-xyzma(1,1)) + normal(2)*( x3dca(2)-xyzma(2,1)) + nor&
                         &mal(3)*(x3dca(3)-xyzma(3,1))
                dmax = 0.0d0
                do 40 inoma = 1, nbcnx
                    dx = x3dca(1) - xyzma(1,inoma)
                    dy = x3dca(2) - xyzma(2,inoma)
                    dz = x3dca(3) - xyzma(3,inoma)
                    d = dble ( sqrt ( dx*dx + dy*dy + dz*dz ) )
                    if (d .gt. dmax) dmax = d
40              continue
                if (dmax .eq. 0.0d0) dmax = 1.0d0
                if (dble(abs(excent))/dmax .lt. epsg) excent = 0.0d0
                call dcopy(3, x3dca(1), 1, x3dp(1), 1)
                if (excent .ne. 0.0d0) then
                    call daxpy(3, -excent, normal(1), 1, x3dp(1),&
                               1)
                    if (excent .lt. 0.0d0) then
                        excent = dble(abs(excent))
                        call dscal(3, -1.0d0, normal(1), 1)
                    endif
                endif
!
!............. TEST D'APPARTENANCE DU POINT PROJETE AU DOMAINE
!............. GEOMETRIQUE DEFINI PAR LA MAILLE
!
                call projtq(nbcnx, xyzma(1, 1), icnx, x3dp(1), itria,&
                            xbar( 1), iproj)
                if (iproj .ge. 0) then
                    goto 9999
                else
                    goto 10
                endif
!
            endif
20      continue
10  end do
!
9999  continue
    call jedema()
!
! --- FIN DE PROJKM.
end subroutine
