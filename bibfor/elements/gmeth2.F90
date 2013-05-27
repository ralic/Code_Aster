subroutine gmeth2(modele, nnoff, ndeg, chthet, fond,&
                  gthi, gs, objcur, xl, gi)
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
    implicit   none
!
! ......................................................................
!      METHODE THETA-LAGRANGE ET G-LEGENDRE POUR LE CALCUL DE G(S)
!
! ENTREE
!
!     MODELE   --> NOM DU MODELE
!     NNOFF    --> NOMBRE DE NOEUDS DU FOND DE FISSURE
!     NDEG     --> DEGRE DES POLYNOMES DE LEGENDRE
!     CHTHET   --> VALEURS DE LA NORMALE SUR LE FOND DE FISSURE
!     FOND     --> NOMS DES NOEUDS DU FOND DE FISSURE
!     GTHI     --> VALEURS DE G POUR LES CHAMPS THETAI
!     OBJCUR   --> ABSCISSES CURVILIGNES S
!
!  SORTIE
!
!      GS      --> VALEUR DE G(S)
!      GI      --> VALEUR DE GI
!......................................................................
!
    include 'jeveux.h'
!
    include 'asterfort/detrsd.h'
    include 'asterfort/glegen.h'
    include 'asterfort/gsyste.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/wkvect.h'
    integer :: nnoff, ndeg, iadrt3, i, j, k, l, m, inorme
    integer :: iabsc, iadint, iadpol, istok, iadnor, iadrno
    integer :: iadnum, iadrma, iadrco, iter1, iter2
!
    real(kind=8) :: t1, t2, s1, s2, xl, som, som1, som2, vks(7), vwi(7)
    real(kind=8) :: x1, y1, z1, x2, y2, z2, prod1, prod2
    real(kind=8) :: gthi(1), gs(1), gi(1)
!
    character(len=8) :: modele, noma1
    character(len=24) :: numgam, obj1, nomno, coorn, chthet, fond
    character(len=24) :: matr, objcur
!
!
!
!
! VALEURS DES COORDONNES DES POINTS DE GAUSS POUR L'INTEGRATION
! NUMERIQUE PAR LA METHODE DE GAUSS ENTRE -1 ET +1 D'UNE INTEGRALE
!
    data vks /-.949107912342759D0,&
     &          -.741531185599394D0,&
     &          -.405845151377397D0,&
     &          0.d0,&
     &           .405845151377397D0,&
     &           .741531185599394D0,&
     &           .949107912342759D0/
!
! VALEURS DES POIDS ASSOCIES
!
    data vwi /.129484966168870D0,&
     &          .279705391489277D0,&
     &          .381830050505119D0,&
     &          .417959183673469D0,&
     &          .381830050505119D0,&
     &          .279705391489277D0,&
     &          .129484966168870D0/
!
    call jemarq()
!
!
! OBJET DECRIVANT LE MAILLAGE
!
    call jeveuo(chthet, 'L', iadnor)
    call jeveuo(fond, 'L', iadrno)
    obj1 = modele//'.MODELE    .LGRF'
    call jeveuo(obj1, 'L', iadrma)
    noma1 = zk8(iadrma)
    nomno = noma1//'.NOMNOE'
    coorn = noma1//'.COORDO    .VALE'
    call jeveuo(coorn, 'L', iadrco)
!
! VALEURS DES POLYNOMES DE LEGENDRE POUR LES NOEUDS DU FOND DE FISSURE
!
    call wkvect('&&METHO2.THETA', 'V V R8', (ndeg+1)*nnoff, iadrt3)
!
    call glegen(ndeg, nnoff, xl, objcur, zr(iadrt3))
!
!- OBJETS DE TRAVAIL
!   COORDONNEES TRANSFORMES DES POINTS DE GAUSS
!   VALEURS DE CES POINTS SUR LES POLYNOMES DE LEGENDRE
!
    call wkvect('&&METHO2.PGAUSS', 'V V R8', 7, iadint)
    call wkvect('&&METHO2.VALPOL', 'V V R8', 7*(ndeg+1), iadpol)
!
!- NUMEROS DES NOEUDS DU FOND DE FISSURE ET NORMALES
!
    numgam = '&&METHO2.NUMGAM'
    call wkvect(numgam, 'V V I', nnoff, iadnum)
    call wkvect('&&METHO2.VALNORM', 'V V R8', 3*nnoff, inorme)
!
    do 10 j = 1, nnoff
        call jenonu(jexnom(nomno, zk8(iadrno+j-1)), zi(iadnum+j-1))
10  end do
!
    do 20 i = 1, nnoff
        zr(inorme+(i-1)*3+1-1) = zr(iadnor+(zi(iadnum+i-1)-1)*3+1-1)
        zr(inorme+(i-1)*3+2-1) = zr(iadnor+(zi(iadnum+i-1)-1)*3+2-1)
        zr(inorme+(i-1)*3+3-1) = zr(iadnor+(zi(iadnum+i-1)-1)*3+3-1)
20  end do
!
!     ABSCISSES CURVILIGNES DES NOEUDS DU FOND DE FISSURE
    call jeveuo(objcur, 'L', iabsc)
!
!- TERMES INTEGRALES ET VALEURS ASSEMBLES DE LA MATRICE
!
    call wkvect('&&METHO2.TERM1', 'V V R8', (nnoff-1)*(ndeg+1), iter1)
    call wkvect('&&METHO2.TERM2', 'V V R8', (nnoff-1)*(ndeg+1), iter2)
    matr = '&&METHO2.MATRIC'
    call wkvect(matr, 'V V R8', nnoff*(ndeg+1), istok)
!
!- BOUCLE SUR LES NOEUDS DU FOND DE FISSURE
!
    do 500 i = 1, nnoff
        do 400 j = 1, ndeg+1
            do 300 k = 1, nnoff-1
!
                s1 = zr(iabsc-1+k)
                s2 = zr(iabsc-1+k+1)
                do 510 l = 1, 7
                    zr(iadint+l-1) = ((s2-s1)*vks(l)+s1+s2)/2d0
510              continue
!
! VALEURS DES POLYNOMES DE LEGENDRE POUR LE DEGRE J-1
!
                call glegen(j-1, 7, xl, '&&METHO2.PGAUSS         ', zr(iadpol))
                som1 = 0.d0
                som2 = 0.d0
!
! TERMES INTEGRALES DE REFERENCES TRANSFORMES EN SOMMATION
!
                do 520 m = 1, 7
                    som1=som1+vwi(m)*zr(iadpol+(j-1)*7+m-1)*(1+vks(m))&
                    /2.d0
                    som2=som2+vwi(m)*zr(iadpol+(j-1)*7+m-1)*(1-vks(m))&
                    /2.d0
520              continue
!
! NORMALES AUX NOEUDS NK ET NK+1
!
                x1 = zr(inorme+(k-1)*3+1-1)
                y1 = zr(inorme+(k-1)*3+2-1)
                z1 = zr(inorme+(k-1)*3+3-1)
                x2 = zr(inorme+(k+1-1)*3+1-1)
                y2 = zr(inorme+(k+1-1)*3+2-1)
                z2 = zr(inorme+(k+1-1)*3+3-1)
!
! PRODUIT SCALAIRE DE LA NORMALE AU POINT D'INTEGRATION ( EGALE
! A LA MOYENNE DES NORMALES NK ET NK+1) ET DE LA NORMALE EN NK
!
                prod1 = ((x1+x2)*x1+(y1+y2)*y1+(z1+z2)*z1)*(s2-s1)/ 4.d0
                prod2 = ((x1+x2)*x2+(y1+y2)*y2+(z1+z2)*z2)*(s2-s1)/ 4.d0
                zr(iter1+(k-1)*(ndeg+1)+j-1)= som1*prod1
                zr(iter2+(k-1)*(ndeg+1)+j-1)= som2*prod2
300          continue
!
            if (i .eq. 1) then
                zr(istok+j-1) = zr(iter2+j-1)
            else if (i.eq.nnoff) then
                t1 = zr(iter1+(nnoff-1-1)*(ndeg+1)+j-1)
                zr(istok+(nnoff-1)*(ndeg+1)+j-1) = t1
            else
                t1 = zr(iter2+(i-1 )*(ndeg+1)+j-1)
                t2 = zr(iter1+(i-1-1)*(ndeg+1)+j-1)
                zr(istok+(i-1)*(ndeg+1)+j-1) = t1+t2
            endif
400      continue
500  end do
!
! RESOLUTION DU SYSTEME LINEAIRE:  MATR*GI = GTHI
!
    call gsyste(matr, ndeg+1, nnoff, gthi, gi)
!
! CALCUL DE G(S)
!
    do 60 i = 1, nnoff
        som = 0.d0
        do 50 j = 1, ndeg+1
            som = som + gi(j)*zr(iadrt3+(j-1)*nnoff+i-1)
50      continue
        gs(i) = som
60  end do
!
! DESTRUCTION DES OBJETS DE TRAVAIL
!
    call jedetr('&&METHO2.THETA')
    call jedetr('&&METHO2.PGAUSS')
    call jedetr('&&METHO2.VALPOL')
    call jedetr('&&METHO2.VALNORM')
    call jedetr('&&METHO2.MATRIC')
    call jedetr('&&METHO2.NUMGAM')
    call jedetr('&&METHO2.TERM1')
    call jedetr('&&METHO2.TERM2')
    call detrsd('CHAMP_GD', '&&GMETH2.G2        ')
!
    call jedema()
end subroutine
