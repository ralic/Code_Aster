subroutine gimpgs(result, nnoff, absc, gs, numero,&
                  gi, ndeg, ndimte, gthi, extim,&
                  time, iordr, unit)
    implicit  none
! ......................................................................
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
!
!     - FONCTION REALISEE:  IMPRESSION DU TAUX DE RESTITUTION D'ENERGIE
!                           LOCAL SUIVANT LA METHODE CHOISIE
!  ENTREE
!
!    RESULT       --> NOM UTILISATEUR DU RESULTAT ET TABLE
!    NNOFF        --> NOMBRE DE NOEUDS DU FOND DE FISSURE
!    ABSC         --> ABSCISSES CURVILIGNES
!    GS           --> VALEURS DE G(S)
!    NUMERO       --> NUMERO DE LA METHODE   1 : THETA-LEGENDRE
!                                            2 : THETA-LAGRANGE
!                                            3 : LAGRANGE-LAGRANGE
!    UNIT         --> UNITE DU FICHIER D'AFFICHAGE
!    GI           --> VALEURS DES GI=<G,THETAI>
!    NDEG         --> DEGRE DU POLYNOME DE LEGENDRE
!    GTHI         --> VALEURS DES G ELEMENTAIRES AVANT LISSAGE
!    EXTIM        --> .TRUE. => TIME EXISTE
!    TIME         --> INSTANT DE CALCUL A IMPRIMER
!    IORDR        --> NUMERO D'ORDRE A IMPRIMER
! ......................................................................
!
    integer :: nnoff, unit, numero, ndeg, iordr, i, i1, ndimte
    real(kind=8) :: gs(1), gthi(1), gi(1), time, absc(*)
    logical :: extim
    character(len=8) :: result
! ......................................................................
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
!
    write(unit,666)
    write(unit,*)
!
    if (numero .ne. 1) then
        write(unit,770)
        write(unit,*)
        write(unit,*) ' NOEUD    GELEM(THETAI)'
        write(unit,*)
        if (numero .eq. 5) then
            do 20 i = 1, ndimte
                write(unit,110) i,gthi(i)
20          continue
        else
            do 21 i = 1, nnoff
                write(unit,110) i,gthi(i)
21          continue
        endif
        write(unit,*)
    endif
!
    if ((numero.eq.1) .or. (numero.eq.2)) then
        write(unit,777)
        write(unit,*)
        do 10 i = 1, ndeg+1
            i1 = i-1
            write(unit,*) 'DEGRE ',i1,' : ',gi(i)
10      continue
        write(unit,*)
    endif
!
    if (extim) then
        write(unit,*) '          INSTANT : ',time
        write(unit,*) '          +++++++'
    else if (iordr.ne.0) then
        write(unit,*) '          NUMERO D''ORDRE : ',iordr
        write(unit,*) '          ++++++++++++++'
    endif
    write(unit,*)
    write(unit,*)  'TAUX DE RESTITUTION D''ENERGIE LOCAL : ',result
    write(unit,*)
    write(unit,*)  ' ABSC_CURV       G(S)'
    write(unit,*)
    do 30 i = 1, nnoff
        write(unit,111) absc(i), gs(i)
30  end do
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
    770 format('VALEURS DE G ELEMENTAIRES AVANT LISSAGE :')
    777 format('COEF DE G(S) DANS LA BASE DE POLYNOMES DE LEGENDRE :')
!
end subroutine
