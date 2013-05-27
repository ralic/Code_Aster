subroutine ffgra1(nbfonc, idebit, nbp1, nbp2, long,&
                  disc, vale)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!     DISCRETISATION DES FONCTIONS DE FORME GRAPPE1
!     APPELANT : SPECFF
!-----------------------------------------------------------------------
! IN  : NBFONC : NOMBRE DE FONCTIONS
! IN  : IDEBIT : INDICE CARACTERISTIQUE DU DEBIT DE REFERENCE
! IN  : NBP1   : NOMBRE DE POINTS DE DISCRETISATION DES FONCTIONS SUR
!                L'INTERVALLE 0,L
! IN  : NBP2   : NOMBRE DE POINTS DE DISCRETISATION DES FONCTIONS SUR
!                L'INTERVALLE L,2L
! IN  : LONG   : VALEUR DE LA LONGUEUR EXCITEE L
! OUT : DISC   : VALEURS DU PARAMETRE SUR 0,2L   - DIM : NBP
! OUT : VALE   : TABLEAU DES VALEURS DES FONCTIONS SUR 0,2L
!                - DIM : (NBP,NBFONC)
!
!
    include 'jeveux.h'
    integer :: nbfonc, idebit, nbp1, nbp2
    real(kind=8) :: long, disc(nbp1+nbp2), vale(nbp1+nbp2, nbfonc)
!
    integer :: nbtron, ideb(3), ifin(3)
    real(kind=8) :: xdeb(3), x0(3), coef18(90), coef30(90)
    real(kind=8) :: lambda
!
!-----------------------------------------------------------------------
    integer :: i, imod, ip, itron, nbmod, nbp
    real(kind=8) :: coef1, coef2, coef3, coef4, dx, x, xref
!
!-----------------------------------------------------------------------
    parameter   (nbtron=3)
!
    data ideb   / 1 , 35 , 68  /
    data ifin   /34 , 67 , 100 /
!
    data xdeb   / .000000D+00, .337000D+00, .653999D+00/
    data x0     / .000000D+00, .405300D+00, .722800D+00/
!
    data (coef18(i),i= 1,15) /&
     & .892004D+01, .876767D-01, .101885D+01,-.864421D-01, .867667D-01,&
     & .892004D+01,-.592371D+00,-.647456D+00, .110354D+00,-.607590D-01,&
     & .892004D+01, .716904D+00, .678938D+00,-.781130D-01, .525716D-01/
    data (coef18(i),i=16,30) /&
     & .958117D+01,-.824166D-01, .722287D+00, .851556D-01,-.900713D-01,&
     & .958117D+01,-.512911D+00,-.647767D+00,-.369576D-02, .255866D-01,&
     & .958117D+01, .607072D+00, .744850D+00,-.966177D-03,-.149861D-01/
    data (coef18(i),i=31,45) /&
     & .104292D+02,-.315180D-01,-.670248D+00, .298443D-01,-.518046D-01,&
     & .104292D+02, .105637D+00,-.255743D+00,-.147931D+00, .199660D+00,&
     & .104292D+02, .393381D+00, .836201D+00, .166871D+00,-.186046D+00/
    data (coef18(i),i=46,60) /&
     & .121346D+02, .146509D-01, .422678D+00,-.147882D-01, .332653D-01,&
     & .121346D+02, .373902D+00, .698009D+00, .216923D+00,-.167900D+00,&
     & .121346D+02, .139962D-01, .498839D+00, .252902D+00,-.259762D+00/
    data (coef18(i),i=61,75) /&
     & .186172D+02,-.130075D+00,-.717144D+00, .129807D+00,-.130585D+00,&
     & .186172D+02,-.963054D+00, .725669D-01, .891863D-02,-.593922D-02,&
     & .186172D+02,-.550072D+00,-.592468D-01, .737335D-01,-.756190D-01/
    data (coef18(i),i=76,90) /&
     & .214486D+02, .192720D+00,-.746667D+00,-.185555D+00, .186725D+00,&
     & .214486D+02, .616604D+00, .191943D+00, .958665D-01,-.986181D-01,&
     & .214486D+02, .552441D+00, .395168D+00, .595057D-01,-.594600D-01/
!
    data (coef30(i),i= 1,15) /&
     & .862668D+01,-.328857D-01, .570796D+00, .323381D-01,-.468150D-01,&
     & .862668D+01,-.376416D+00,-.610767D+00, .388797D-01, .500452D-01,&
     & .862668D+01, .670443D+00, .688077D+00,-.798460D-01, .279372D-01/
    data (coef30(i),i=16,30) /&
     & .931612D+01,-.725227D-01, .691873D+00, .729040D-01,-.819260D-01,&
     & .931612D+01,-.502377D+00,-.642044D+00, .129275D-01, .202009D-01,&
     & .931612D+01, .619794D+00, .692713D+00,-.370607D-01, .180415D-01/
    data (coef30(i),i=31,45) /&
     & .102233D+02, .147741D-01,-.742058D+00,-.146033D-01,-.120140D-02,&
     & .102233D+02, .290293D+00,-.192318D+00,-.164685D+00, .240156D+00,&
     & .102233D+02, .369431D+00, .797258D+00, .159602D+00,-.186411D+00/
    data (coef30(i),i=46,60) /&
     & .119350D+02, .756823D-01, .457934D+00,-.761885D-01, .959711D-01,&
     & .119350D+02, .329857D+00, .697145D+00, .202018D+00,-.158517D+00,&
     & .119350D+02,-.476558D-01, .465654D+00, .260041D+00,-.275460D+00/
    data (coef30(i),i=61,75) /&
     & .178865D+02,-.197756D+00,-.100842D+01, .196719D+00,-.196414D+00,&
     & .178865D+02,-.609465D+00, .974086D-01, .123622D+00,-.125797D+00,&
     & .178865D+02,-.574084D+00,-.109550D-01, .456200D-01,-.494731D-01/
    data (coef30(i),i=76,90) /&
     & .198739D+02, .571860D+00,-.643644D+00,-.572587D+00, .573938D+00,&
     & .198739D+02, .666135D+00, .365642D+00, .256218D+00,-.258155D+00,&
     & .198739D+02, .336904D+00, .165036D+00,-.582536D-01, .594626D-01/
!
!-----------------------------------------------------------------------
!
    nbp = nbp1 + nbp2
!
! --- 1.VALEURS DU PARAMETRE
!
    dx = 1.d-2
    x = dx
    do 10 itron = 1, nbtron
        disc(ideb(itron)) = xdeb(itron)
        do 11 ip = ideb(itron)+1, ifin(itron)
            disc(ip) = x
            x = x + dx
11      continue
10  end do
    disc(nbp1) = long
!
    do 20 ip = nbp1+1, nbp
        disc(ip) = disc(ip-nbp1) + long
20  end do
!
! --- 2.VALEURS DES FONCTIONS
!
    nbmod = nbfonc/2
!
    if (idebit .eq. 180) then
!
        do 30 imod = 1, nbmod
            do 31 itron = 1, nbtron
                lambda = coef18(15*(imod-1)+5*(itron-1)+1)
                coef1 = coef18(15*(imod-1)+5*(itron-1)+2)
                coef2 = coef18(15*(imod-1)+5*(itron-1)+3)
                coef3 = coef18(15*(imod-1)+5*(itron-1)+4)
                coef4 = coef18(15*(imod-1)+5*(itron-1)+5)
                xref = x0(itron)
                do 32 ip = ideb(itron), ifin(itron)
                    x = lambda*(disc(ip)-xref)
                    vale(ip,imod) = coef1*dble(&
                                    cos(x)) + coef2*dble( sin(x)) + coef3*dble(cosh(x)) + coef4*d&
                                    &ble(sinh(x)&
                                    )
                    vale(ip+nbp1,imod+nbmod) = vale(ip,imod)
32              continue
31          continue
            x = lambda*(disc(nbp1)-xref)
            vale(nbp1,imod) = coef1*dble(&
                              cos(x)) + coef2*dble(sin(x)) + coef3*dble(cosh(x)) + coef4*dble(sin&
                              &h(x)&
                              )
            vale(nbp,imod+nbmod) = vale(nbp1,imod)
30      continue
!
    else
!
        do 40 imod = 1, nbmod
            do 41 itron = 1, nbtron
                lambda = coef30(15*(imod-1)+5*(itron-1)+1)
                coef1 = coef30(15*(imod-1)+5*(itron-1)+2)
                coef2 = coef30(15*(imod-1)+5*(itron-1)+3)
                coef3 = coef30(15*(imod-1)+5*(itron-1)+4)
                coef4 = coef30(15*(imod-1)+5*(itron-1)+5)
                xref = x0(itron)
                do 42 ip = ideb(itron), ifin(itron)
                    x = lambda*(disc(ip)-xref)
                    vale(ip,imod) = coef1*dble(&
                                    cos(x)) + coef2*dble( sin(x)) + coef3*dble(cosh(x)) + coef4*d&
                                    &ble(sinh(x)&
                                    )
                    vale(ip+nbp1,imod+nbmod) = vale(ip,imod)
42              continue
41          continue
            x = lambda*(disc(nbp1)-xref)
            vale(nbp1,imod) = coef1*dble(&
                              cos(x)) + coef2*dble(sin(x)) + coef3*dble(cosh(x)) + coef4*dble(sin&
                              &h(x)&
                              )
            vale(nbp,imod+nbmod) = vale(nbp1,imod)
40      continue
!
    endif
!
    do 50 imod = 1, nbmod
        do 51 ip = 1, nbp1
            vale(ip+nbp1,imod) = 0.d0
            vale(ip,imod+nbmod) = 0.d0
51      continue
50  end do
!
end subroutine
