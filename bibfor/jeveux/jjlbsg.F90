subroutine jjlbsg(ic, id, ioc, ibacol, iadmi,&
                  iadyn, ltot)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ----------------------------------------------------------------------
!
! LIBERE LE SEGMENT DE VALEURS ASSOCIES A UN OBJET SIMPLE OU UN OBJET
! DE COLLECTION DISPERSEE
!
! IN     IC   : CLASSE ASSOCIEE A L'OBJET
! IN     ID   : IDENTIFICATEUR D'OBJET SIMPLE OU DE COLLECTIOB
! IN    IOC   : IDENTIFICATUER DE COLLECTION
! IN IBACOL   ; ADDRESSE DU DESCRIPTEUR DE COLLECTION
! IN  IADMI   : ADDRESSE DE L'OBJET A LIBERER
! IN  IADYN   : ADDRESSE DYNAMIQUE DE L'OBJET A LIBERER
! OUT   LTOT  : LONGUEUR EN ENTIERS LIBEREE
!
! ----------------------------------------------------------------------
! person_in_charge: j-pierre.lefebvre at edf.fr
    implicit none
#include "jeveux_private.h"
#include "asterc/hpdeallc.h"
#include "asterfort/jxecro.h"
    integer :: ic, id, ioc, ibacol, iadmi, iadyn, ltot
!     ------------------------------------------------------------------
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
!-----------------------------------------------------------------------
    integer :: ibiadd, ibiadm, iblono, ixdeso, ixiadd
    integer :: ixiadm, ixlono, jcara, jdate, jhcod, jiacce, jiadd
    integer :: jiadm, jlong, jlono, jltyp, jluti, jmarq, lonoi
    integer :: lsv, n, nbacce
!-----------------------------------------------------------------------
    parameter  ( n = 5 )
    common /jiatje/  jltyp(n), jlong(n), jdate(n), jiadd(n), jiadm(n),&
     &                 jlono(n), jhcod(n), jcara(n), jluti(n), jmarq(n)
!
    integer :: nblmax, nbluti, longbl, kitlec, kitecr, kiadm, iitlec, iitecr
    integer :: nitecr, kmarq
    common /ificje/  nblmax(n) , nbluti(n) , longbl(n) ,&
     &                 kitlec(n) , kitecr(n) ,             kiadm(n) ,&
     &                 iitlec(n) , iitecr(n) , nitecr(n) , kmarq(n)
    common /jiacce/  jiacce(n),nbacce(2*n)
    integer :: ldyn, lgdyn, nbdyn, nbfree
    common /idynje/  ldyn , lgdyn , nbdyn , nbfree
    integer :: icdyn, mxltot
    common /xdynje/  icdyn , mxltot
    real(kind=8) :: mxdyn, mcdyn, mldyn, vmxdyn, vmet, lgio, cuvtrav
    common /r8dyje/ mxdyn, mcdyn, mldyn, vmxdyn, vmet, lgio(2), cuvtrav
    integer :: isstat
    common /iconje/  isstat
    integer :: lbis, lois, lols, lor8, loc8
    common /ienvje/  lbis , lois , lols , lor8 , loc8
! ----------------------------------------------------------------------
    integer :: iddeso, idiadd, idiadm, idlono
    parameter    (  iddeso = 1 , idiadd = 2 , idiadm = 3 ,&
     &               idlono = 8    )
! ----------------------------------------------------------------------
    integer :: idm, isd, isf, il, ltypi, lgs, iaddi(2), nbioav(2)
!
    nbioav(1) = nbacce(2*ic-1)
    nbioav(2) = nbacce(2*ic )
!
    if (ioc .eq. 0) then
!
!     ON TRAITE UN OBJET SIMPLE OU UN OBJET $$DESO
!
        if (iadyn .ne. 0) then
            idm = iadmi - 4
            isd = iszon(jiszon + idm + 3) / isstat
            isf = iszon(jiszon + iszon(jiszon+idm) - 4) / isstat
            il = iszon(jiszon+idm) - 8 - idm
            if (isd .eq. 1) then
!
!     LE SEGMENT DE VALEURS EST MARQUE X A OU X D, ON PEUT LE LIBERER
!
                ltypi = ltyp( jltyp(ic)+id )
                lsv = lono( jlono(ic)+id ) * ltypi
                if (isf .eq. 4) then
!
!     LE SEGMENT DE VALEURS EST MARQUE X D, IL FAUT D'ABORD L'ECRIRE
!
                    iaddi(1) = iadd ( jiadd(ic)+2*id-1 )
                    iaddi(2) = iadd ( jiadd(ic)+2*id )
                    call jxecro(ic, iadmi, iaddi, lsv, 0,&
                                id)
                    iadd( jiadd(ic)+2*id-1 ) = iaddi(1)
                    iadd( jiadd(ic)+2*id ) = iaddi(2)
                endif
                lgs = iszon(jiszon+iadmi-4) - iadmi + 4
                mcdyn = mcdyn - lgs
                mldyn = mldyn + lgs
                call hpdeallc(iadyn, nbfree)
                ltot = ltot + il
                iadm(jiadm(ic)+2*id-1) = 0
                iadm(jiadm(ic)+2*id ) = 0
            endif
        endif
    else
!
!     ON TRAITE UN OBJET DE COLLECTION DISPERSEE
!
        ixiadm = iszon ( jiszon + ibacol + idiadm )
        ixiadd = iszon ( jiszon + ibacol + idiadd )
        ixdeso = iszon ( jiszon + ibacol + iddeso )
        ixlono = iszon ( jiszon + ibacol + idlono )
        if (ixiadm .gt. 0) then
            ibiadm = iadm ( jiadm(ic) + 2*ixiadm-1 )
            ibiadd = iadm ( jiadm(ic) + 2*ixiadd-1 )
            if (iadyn .ne. 0) then
                idm = iadmi - 4
                isd = iszon(jiszon + idm + 3) / isstat
                isf = iszon(jiszon + iszon(jiszon+idm) - 4) / isstat
                il = iszon(jiszon+idm) - 8 - idm
                if (isd .eq. 1) then
!
!     LE SEGMENT DE VALEURS EST MARQUE X A OU X D, ON PEUT LE LIBERER
!
                    if (ixlono .ne. 0) then
                        iblono = iadm ( jiadm(ic) + 2*ixlono-1 )
                        lonoi = iszon(jiszon + iblono - 1 + ioc)
                    else
                        lonoi = lono(jlono(ic)+ ixdeso)
                    endif
                    ltypi = ltyp( jltyp(ic)+ixdeso )
                    lsv = lonoi * ltypi
                    if (isf .eq. 4) then
!
!     LE SEGMENT DE VALEURS EST MARQUE X D, IL FAUT D'ABORD L'ECRIRE
!
                        iaddi(1) = iszon(jiszon + ibiadd -1 + 2*ioc-1)
                        iaddi(2) = iszon(jiszon + ibiadd -1 + 2*ioc )
                        call jxecro(ic, iadmi, iaddi, lsv, id,&
                                    ioc)
                        iszon(jiszon + ibiadd -1 + 2*ioc-1) = iaddi(1)
                        iszon(jiszon + ibiadd -1 + 2*ioc ) = iaddi(2)
                    endif
                    lgs = iszon(jiszon+iadmi-4) - iadmi + 4
                    mcdyn = mcdyn - lgs
                    mldyn = mldyn + lgs
                    call hpdeallc(iadyn, nbfree)
                    ltot = ltot + il
                    iszon(jiszon + ibiadm - 1 +2*ioc-1) = 0
                    iszon(jiszon + ibiadm - 1 +2*ioc ) = 0
                endif
            endif
        endif
    endif
!
    lgio(1)=lgio(1)+1024*longbl(ic)*lois*(nbacce(2*ic-1)-nbioav(1))
    lgio(2)=lgio(2)+1024*longbl(ic)*lois*(nbacce(2*ic  )-nbioav(2))
!
    mxltot=mxltot+(ltot*lois)/(1024*1024)
!
end subroutine
