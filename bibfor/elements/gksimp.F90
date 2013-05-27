subroutine gksimp(result, nnoff, absc, iadrgk, numero,&
                  iadgks, ndeg, ndimte, iadgki, extim,&
                  time, iordr, unit)
    implicit  none
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
!
!     - FONCTION REALISEE:  IMPRESSION DU TAUX DE RESTITUTION D'ENERGIE
!                           ET DES FACTEURS D'INTENSITES DE CONTRAINTES
!                           LOCALS SUIVANT LA METHODE CHOISIE
!                           DANS LE CADRE DE X-FEM
!  ENTREE
!
!    RESULT       --> NOM UTILISATEUR DU RESULTAT ET TABLE
!    NNOFF        --> NOMBRE DE POINTS DU FOND DE FISSURE
!    ABSC         --> ABSCISSES CURVILIGNES
!    IADRGK       --> ADRESSE DE VALEURS DE GKTHI
!                 (G, K1, K2, K3, BETA POUR LES CHAMPS THETAI)
!
!    NUMERO       --> NUMERO DE LA METHODE   1 : THETA-LEGENDRE
!                                            2 : THETA-LAGRANGE
!                                            3 ou 4 : LAGRANGE-LAGRANGE
!    IADGKS       --> ADRESSE DE VALEURS DE GKS
!                      (VALEUR DE G(S), K1(S), K2(S), K3(S), BETA(S))
!    NDEG         --> DEGRE DU POLYNOME DE LEGENDRE
!    IADGKI       --> ADRESSE DES VALEURS ELEMENTAIRES AVANT LISSAGE
!                      (VALEUR DE G(S), K1(S), K2(S), K3(S))
!    EXTIM        --> .TRUE. => TIME EXISTE
!    TIME         --> INSTANT DE CALCUL A IMPRIMER
!    IORDR        --> NUMERO D'ORDRE A IMPRIMER
!    UNIT         --> UNITE DU FICHIER D'AFFICHAGE
! ......................................................................
!
    include 'jeveux.h'
!
    integer :: nnoff, unit, numero, ndeg, iordr, i, i1, imod
    integer :: iadrgk, iadgks, iadgki, ndimte
    real(kind=8) :: time, absc(*)
    logical :: extim
    character(len=8) :: result
!
!
!
!
    write(unit,*)
!
    if (numero .eq. 1) then
        write(unit,555) ndeg
    else if (numero.eq.2) then
        write(unit,556) ndeg
    else if (numero.eq.3) then
        write(unit,557)
    else if (numero.eq.4) then
        write(unit,558)
    else if (numero.eq.5) then
        write(unit,559)
    endif
    write(unit,666)
    write(unit,*)
!
    write(unit,*)  'NOM DU RESULTAT : ',result
    write(unit,*)
    if (extim) then
        write(unit,*) '          INSTANT : ',time
        write(unit,*) '          +++++++'
    else if (iordr.ne.0) then
        write(unit,*) '          NUMERO D''ORDRE : ',iordr
        write(unit,*) '          ++++++++++++++'
    endif
    write(unit,*)
!
!- IMPRESSION DU TAUX DE RESTITUTION D ENERGIE G
!
    write(unit,*) 'TAUX DE RESTITUTION D''ENERGIE LOCAL G'
    write(unit,667)
!
    if (numero .ne. 1) then
        write(unit,770)
        write(unit,*)
        write(unit,*) ' NOEUD    GELEM(THETAI)'
        write(unit,*)
        if (numero .eq. 5) then
            do 20 i = 1, ndimte
                write(unit,110) i,zr(iadgki-1+(i-1)*5+1)
20          continue
        else
            do 21 i = 1, nnoff
                write(unit,110) i,zr(iadgki-1+(i-1)*5+1)
21          continue
        endif
        write(unit,*)
    endif
!
    if ((numero.eq.1) .or. (numero.eq.2)) then
        write(unit,771)
        write(unit,*)
        do 10 i = 1, ndeg+1
            i1 = i-1
            write(unit,*) 'DEGRE ',i1,' : ',zr(iadgki-1+i1*5+1)
10      continue
        write(unit,*)
    endif
!
    write(unit,*) 'VALEUR DE G AUX POINTS DE FOND DE FISSURE'
    write(unit,*)  ' ABSC_CURV       G(S)'
    write(unit,*)
    do 30 i = 1, nnoff
        write(unit,111) absc(i), zr(iadgks-1+(i-1)*6+1)
30  end do
    write(unit,*)
!
!
!- IMPRESSION DES FACTEURS D INTENSITE DE CONTRAINTE K
!
    do 40 imod = 1, 3
        write(unit,750) imod
        write(unit,667)
        if (numero .ne. 1) then
            write(unit,772) imod
            write(unit,*)
            write(unit,773) imod
            write(unit,*)
            if (numero .eq. 5) then
                do 401 i = 1, ndimte
                    write(unit,110) i,zr(iadgki-1+(i-1)*5+imod+1)
401              continue
            else
                do 402 i = 1, nnoff
                    write(unit,110) i,zr(iadgki-1+(i-1)*5+imod+1)
402              continue
            endif
            write(unit,*)
        endif
        if ((numero.eq.1) .or. (numero.eq.2)) then
            write(unit,751) imod
            write(unit,*)
            do 41 i = 1, ndeg+1
                i1 = i-1
                write(unit,*) 'DEGRE ',i1,' : ',zr(iadgki-1+i1*5+&
                imod+1)
41          continue
            write(unit,*)
        endif
!
        write(unit,752) imod
        write(unit,753) imod
        write(unit,*)
        do 42 i = 1, nnoff
            write(unit,111) absc(i), zr(iadgks-1+(i-1)*6+imod+1)
42      continue
        write(unit,*)
40  end do
!
!- IMPRESSION DE L'ANGLE DE PROPAGATION DE FISSURE BETA
!
    write(unit,*) 'ANGLE DE PROPAGATION DE FISSURE BETA'
    write(unit,667)
!
    write(unit,*) 'VALEUR DE BETA AUX POINTS DE FOND DE FISSURE'
    write(unit,*)  ' ABSC_CURV       BETA(S)'
    write(unit,*)
    do 50 i = 1, nnoff
        write(unit,111) absc(i), zr(iadgks-1+(i-1)*6+6)
50  end do
    write(unit,*)
!
!- IMPRESSION DE G_IRWIN
!
    write(unit,*) 'G_IRWIN (RECALCULE A PARTIR DE K1, K2 ET K3)'
    write(unit,667)
!
    write(unit,*) 'VALEUR DE G_IRWIN AUX POINTS DE FOND DE FISSURE'
    write(unit,*)  ' ABSC_CURV       G_IRWIN(S)'
    write(unit,*)
    do 60 i = 1, nnoff
        write(unit,111) absc(i), zr(iadgks-1+(i-1)*6+5)
60  end do
    write(unit,*)
!
    110 format(1x,i2,6x,1pd12.5)
    111 format(1x,2(1pd12.5,4x))
    555 format('THETA_LEGENDRE  G_LEGENDRE (DEGRE ',i2,')')
    556 format('THETA_LAGRANGE  G_LEGENDRE (DEGRE ',i2,')')
    557 format('THETA_LAGRANGE  G_LAGRANGE')
    558 format('THETA_LAGRANGE  G_LAGRANGE_NO_NO')
    559 format('THETA_LAGRANGE_REGU  G_LAGRANGE_REGU')
    666 format(37('*'))
    667 format(37('+'))
!
    750 format('FACTEUR D''INTENSITE DE CONTRAINTE K',i1)
    751 format('COEF DE K',i1,'(S) DANS LA BASE DE POLY DE LEGENDRE : ')
    752 format('VALEUR DE K',i1,' AUX POINTS DE FOND DE FISSURE')
    753 format(' ABSC_CURV       K',i1,'(S)')
!
    770 format('VALEURS DE G ELEMENTAIRES AVANT LISSAGE :')
    771 format('COEF DE G(S) DANS LA BASE DE POLYNOMES DE LEGENDRE :')
    772 format('VALEURS DE K',i1,' ELEMENTAIRES AVANT LISSAGE :')
    773 format(' NOEUD    K',i1,'ELEM(THETAI)')
!
end subroutine
