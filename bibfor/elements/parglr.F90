subroutine parglr(nlit, elb, ea, nua, liner,&
                  omx, omy, rx, ry, hh,&
                  bn11, bn12, bn22, bn33, bm11,&
                  bm12, bm22, bc11, bc22)
    implicit   none
    integer :: nlit
    real(kind=8) :: elb(*), ea(*), nua(*), liner(*), omx(*), omy(*), rx(*)
    real(kind=8) :: ry(*)
    real(kind=8) :: bn11, bn12, bn22, bn33, bm11, bm12, bm22, bc11, bc22, hh
!.......................................................................
!            CONFIGURATION MANAGEMENT OF EDF VERSION
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
    real(kind=8) :: esn, eox, eoy, neo, eoo, erox, eroy, nero, eroo, er2ox
    real(kind=8) :: er2oy, ner2o
    real(kind=8) :: er2oo, eb, nub
    integer :: ii
!
!
    eb = elb(1)
    nub= elb(2)
!
    esn=eb/(1-nub**2)
    eox=0.d0
    eoy=0.d0
    neo=0.d0
    eoo=0.d0
    erox=0.d0
    eroy=0.d0
    nero=0.d0
    eroo=0.d0
    er2ox=0.d0
    er2oy=0.d0
    ner2o=0.d0
    er2oo=0.d0
    do 33, ii=1,nlit
    eox=eox+ea(ii)*omx(ii)/(1.d0-nua(ii)**2)
    eoy=eoy+ea(ii)*omy(ii)/(1.d0-nua(ii)**2)
    neo=neo+nua(ii)*ea(ii)*(omx(ii)+omy(ii))/2.d0/(1.d0-nua(ii)**&
        2)
    eoo=eoo+liner(ii)*ea(ii) *(omx(ii)+omy(ii))/2.d0/(1.d0+nua(ii)&
        )/2.d0
    erox=erox+ea(ii)*rx(ii)*omx(ii)/(1.d0-nua(ii)**2)
    eroy=eroy+ea(ii)*ry(ii)*omy(ii)/(1.d0-nua(ii)**2)
    nero=nero+nua(ii)*ea(ii) *(rx(ii)+ry(ii))/2.d0 *(omx(ii)+omy(&
        ii))/2.d0/(1.d0-nua(ii)**2)
    eroo=eroo+liner(ii)*ea(ii) *(rx(ii)+ry(ii))/2.d0 *(omx(ii)+&
        omy(ii))/2.d0/(1.d0+nua(ii))/2.d0
    er2ox=er2ox+ea(ii)*rx(ii)**2*omx(ii)/(1.d0-nua(ii)**2)
    er2oy=er2oy+ea(ii)*ry(ii)**2*omy(ii)/(1.d0-nua(ii)**2)
    ner2o=ner2o+nua(ii)*ea(ii) *((rx(ii)+ry(ii))/2.d0)**2 *(omx(&
        ii)+omy(ii))/2.d0/(1.d0-nua(ii)**2)
    er2oo=er2oo+liner(ii)*ea(ii) *((rx(ii)+ry(ii))/2.d0)**2&
        *(omx(ii)+omy(ii))/2.d0/(1.d0+nua(ii))/2.d0
    33 end do
    bn11=    esn * hh       + eox
    bn12=nub*esn * hh       + neo
    bn22=    esn * hh       + eoy
    bn33=eb*hh/(1+nub)/2.d0 + eoo
!
!-----ELASTICITE ORTHOTROPE NON ACTIVEE
    bc11=erox * hh/2.d0
!       BC12=NERO * HH/2.D0
    bc22=eroy * hh/2.d0
!       BC33=EROO * HH/2.D0
    bm11=    esn * hh**3/12.d0 + er2ox*hh**2/4.d0
    bm12=nub*esn * hh**3/12.d0 + ner2o*hh**2/4.d0
    bm22=    esn * hh**3/12.d0 + er2oy*hh**2/4.d0
!       BM33=EB*HH**3/(1+NUB)/24.D0+ ER2OO*HH**2/4.D0
end subroutine
