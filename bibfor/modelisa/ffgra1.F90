subroutine ffgra1(nbfonc, idebit, nbp1, nbp2, long,&
                  disc, vale)
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
    data xdeb   / .000000d+00, .337000d+00, .653999d+00/
    data x0     / .000000d+00, .405300d+00, .722800d+00/
!
    data (coef18(i),i= 1,15) /&
     & .892004d+01, .876767d-01, .101885d+01,-.864421d-01, .867667d-01,&
     & .892004d+01,-.592371d+00,-.647456d+00, .110354d+00,-.607590d-01,&
     & .892004d+01, .716904d+00, .678938d+00,-.781130d-01, .525716d-01/
    data (coef18(i),i=16,30) /&
     & .958117d+01,-.824166d-01, .722287d+00, .851556d-01,-.900713d-01,&
     & .958117d+01,-.512911d+00,-.647767d+00,-.369576d-02, .255866d-01,&
     & .958117d+01, .607072d+00, .744850d+00,-.966177d-03,-.149861d-01/
    data (coef18(i),i=31,45) /&
     & .104292d+02,-.315180d-01,-.670248d+00, .298443d-01,-.518046d-01,&
     & .104292d+02, .105637d+00,-.255743d+00,-.147931d+00, .199660d+00,&
     & .104292d+02, .393381d+00, .836201d+00, .166871d+00,-.186046d+00/
    data (coef18(i),i=46,60) /&
     & .121346d+02, .146509d-01, .422678d+00,-.147882d-01, .332653d-01,&
     & .121346d+02, .373902d+00, .698009d+00, .216923d+00,-.167900d+00,&
     & .121346d+02, .139962d-01, .498839d+00, .252902d+00,-.259762d+00/
    data (coef18(i),i=61,75) /&
     & .186172d+02,-.130075d+00,-.717144d+00, .129807d+00,-.130585d+00,&
     & .186172d+02,-.963054d+00, .725669d-01, .891863d-02,-.593922d-02,&
     & .186172d+02,-.550072d+00,-.592468d-01, .737335d-01,-.756190d-01/
    data (coef18(i),i=76,90) /&
     & .214486d+02, .192720d+00,-.746667d+00,-.185555d+00, .186725d+00,&
     & .214486d+02, .616604d+00, .191943d+00, .958665d-01,-.986181d-01,&
     & .214486d+02, .552441d+00, .395168d+00, .595057d-01,-.594600d-01/
!
    data (coef30(i),i= 1,15) /&
     & .862668d+01,-.328857d-01, .570796d+00, .323381d-01,-.468150d-01,&
     & .862668d+01,-.376416d+00,-.610767d+00, .388797d-01, .500452d-01,&
     & .862668d+01, .670443d+00, .688077d+00,-.798460d-01, .279372d-01/
    data (coef30(i),i=16,30) /&
     & .931612d+01,-.725227d-01, .691873d+00, .729040d-01,-.819260d-01,&
     & .931612d+01,-.502377d+00,-.642044d+00, .129275d-01, .202009d-01,&
     & .931612d+01, .619794d+00, .692713d+00,-.370607d-01, .180415d-01/
    data (coef30(i),i=31,45) /&
     & .102233d+02, .147741d-01,-.742058d+00,-.146033d-01,-.120140d-02,&
     & .102233d+02, .290293d+00,-.192318d+00,-.164685d+00, .240156d+00,&
     & .102233d+02, .369431d+00, .797258d+00, .159602d+00,-.186411d+00/
    data (coef30(i),i=46,60) /&
     & .119350d+02, .756823d-01, .457934d+00,-.761885d-01, .959711d-01,&
     & .119350d+02, .329857d+00, .697145d+00, .202018d+00,-.158517d+00,&
     & .119350d+02,-.476558d-01, .465654d+00, .260041d+00,-.275460d+00/
    data (coef30(i),i=61,75) /&
     & .178865d+02,-.197756d+00,-.100842d+01, .196719d+00,-.196414d+00,&
     & .178865d+02,-.609465d+00, .974086d-01, .123622d+00,-.125797d+00,&
     & .178865d+02,-.574084d+00,-.109550d-01, .456200d-01,-.494731d-01/
    data (coef30(i),i=76,90) /&
     & .198739d+02, .571860d+00,-.643644d+00,-.572587d+00, .573938d+00,&
     & .198739d+02, .666135d+00, .365642d+00, .256218d+00,-.258155d+00,&
     & .198739d+02, .336904d+00, .165036d+00,-.582536d-01, .594626d-01/
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
