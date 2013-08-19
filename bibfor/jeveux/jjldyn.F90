subroutine jjldyn(imode, lmin, ltot)
! person_in_charge: j-pierre.lefebvre at edf.fr
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
    implicit none
#include "jeveux_private.h"
#include "asterc/hpdeallc.h"
#include "asterfort/jermxd.h"
#include "asterfort/jxecro.h"
#include "asterfort/mpicm0.h"
#include "asterfort/random.h"
#include "asterfort/u2mesr.h"
#include "asterfort/utgtme.h"
#include "asterfort/utptme.h"
#include "asterfort/uttcpu.h"
    integer :: imode, lmin, ltot
! ----------------------------------------------------------------------
! LIBERE LES SEGMENTS DE VALEURS ALLOUES DYNAMIQUEMENT
!
! IN   IMODE :
!              =1 ON NE TRAITE QUE LA BASE VOLATILEC
!              =2 ON NE TRAITE QUE LES OBJETS XA
!              =3 ON NE TRAITE QUE LES OBJETS XA DE LA BASE VOLATILE
!              SINON ON EXAMINE TOUTE LA MEMOIRE
! IN   LMIN  : TAILLE MINIMUM EN ENTIERS REQUISE
!              =< 0 ON LIBERE TOUT
!              =-2  ON LIBERE TOUT MAIS ON N'ACTUALISE PAS VMXDYN
! OUT  LTOT  : LONGUEUR CUMULEE EN ENTIERS DES SEGMENTS DESALLOUES
!
! ----------------------------------------------------------------------
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, iadmi, iadmoc, iadyn, iadyoc, ibacol
    integer :: ibiadd, ibiadm, iblono, ic, idm
    integer :: isd, isdc, isf, ixdeso, ixiadd, ixiadm
    integer :: ixlono, j, jcara, jdate, jdocu, jgenr, jhcod
    integer :: jiacce, jiadd, jiadm, jindir, jj, jlong, jlono
    integer :: jltyp, jluti, jmarq, jorig, jrnom, jtype, k
    integer :: lonoi, lsv, ltypi, n, nbacce, ncla1, ncla2
    integer :: nmax
!-----------------------------------------------------------------------
    parameter  ( n = 5 )
    common /jiatje/  jltyp(n), jlong(n), jdate(n), jiadd(n), jiadm(n),&
     &                 jlono(n), jhcod(n), jcara(n), jluti(n), jmarq(n)
    integer :: nblmax, nbluti, longbl, kitlec, kitecr, kiadm, iitlec, iitecr
    integer :: nitecr, kmarq
    common /ificje/  nblmax(n) , nbluti(n) , longbl(n) ,&
     &                 kitlec(n) , kitecr(n) ,             kiadm(n) ,&
     &                 iitlec(n) , iitecr(n) , nitecr(n) , kmarq(n)
    character(len=2) :: dn2
    character(len=5) :: classe
    character(len=8) :: nomfic, kstout, kstini
    common /kficje/  classe    , nomfic(n) , kstout(n) , kstini(n) ,&
     &                 dn2(n)
    common /jkatje/  jgenr(n), jtype(n), jdocu(n), jorig(n), jrnom(n)
!
    integer :: nrhcod, nremax, nreuti
    common /icodje/  nrhcod(n) , nremax(n) , nreuti(n)
    common /jiacce/  jiacce(n),nbacce(2*n)
    common /jindir/  jindir(n)
    integer :: isstat
    common /iconje/  isstat
    integer :: ldyn, lgdyn, nbdyn, nbfree
    common /idynje/  ldyn , lgdyn , nbdyn , nbfree
    integer :: icdyn, mxltot
    common /xdynje/  icdyn , mxltot
    real(kind=8) :: mxdyn, mcdyn, mldyn, vmxdyn, vmet, lgio
    common /r8dyje/ mxdyn, mcdyn, mldyn, vmxdyn, vmet, lgio(2)
    integer :: lbis, lois, lols, lor8, loc8
    common /ienvje/  lbis , lois , lols , lor8 , loc8
    integer :: datei
    common /iheuje/  datei
! ----------------------------------------------------------------------
    integer :: ivnmax, iddeso, idiadd, idiadm, idlono
    parameter    ( ivnmax = 0 , iddeso = 1 , idiadd = 2 , idiadm = 3 ,&
     &               idlono = 8    )
! ----------------------------------------------------------------------
    character(len=1) :: cgenr
    character(len=8) :: nomk(5)
!     CHARACTER*32   NOM32
    integer :: iaddi(2), lgs, nbioav(2)
    integer :: rang, nbproc, iret, iret2
    real(kind=8) :: graine, valp(5), vx(2), v0
!
    data nomk /'COUR_JV ','RLQ_MEM ','VMSIZE  ','MEM_TOTA','LIMIT_JV'/
!
    call uttcpu('CPU.MEMD.1', 'DEBUT', ' ')
!
!     ON LISTE LES OBJETS ALLOUES DYNAMIQUEMENT EN BALAYANT
!     L'ENSEMBLE DES OBJETS, EN COMMENCANT PAR LA BASE VOLATILE
!
    icdyn = icdyn+1
    ltot = 0
    ncla1 = 1
    ncla2 = index ( classe , '$' ) - 1
    if (ncla2 .lt. 0) ncla2 = n
    if (imode .eq. 1 .or. imode .eq. 3) then
        ncla2 = index ( classe , 'V' )
        ncla1 = ncla2
    endif
    do 200 ic = ncla2, ncla1, - 1
        if (nreuti(ic) .eq. 0) goto 200
        call mpicm0(rang, nbproc)
        if (rang .ne. 0) then
            graine = (rang+1)*datei*1.5d0
            do 202 i = 2, nreuti(ic)
                call random(graine)
                k = int(graine*i)+1
                j = indir(jindir(ic)+i)
                indir(jindir(ic)+i) = indir(jindir(ic)+k)
                indir(jindir(ic)+k) = j
202          continue
        endif
!
        nbioav(1) = nbacce(2*ic-1)
        nbioav(2) = nbacce(2*ic )
        do 205 jj = 1, nreuti(ic)
            j = indir(jindir(ic)+jj)
            iadmi = iadm(jiadm(ic)+2*j-1)
            if (iadmi .eq. 0) goto 205
            iadyn = iadm(jiadm(ic)+2*j )
            cgenr = genr(jgenr(ic)+j)
!          NOM32 = RNOM(JRNOM(IC)+J)
!
!    ISD DESIGNE LE STATUT DE LA COLLECTION
!        =U ON PASSE PAR LES ROUTINES HABITUELLES (JJALLC, JJLIDE)
!        =X ON TRAITE DIRECTEMENT
!
            isdc = iszon(jiszon + iadmi - 1) / isstat
            if (cgenr .eq. 'X' .and. isdc .eq. 2) then
                ibacol = iadmi
                ixiadm = iszon ( jiszon + ibacol + idiadm )
                ixiadd = iszon ( jiszon + ibacol + idiadd )
                ixdeso = iszon ( jiszon + ibacol + iddeso )
                ixlono = iszon ( jiszon + ibacol + idlono )
                nmax = iszon ( jiszon + ibacol + ivnmax )
                if (ixiadm .gt. 0) then
                    ibiadm = iadm ( jiadm(ic) + 2*ixiadm-1 )
                    ibiadd = iadm ( jiadm(ic) + 2*ixiadd-1 )
                    do 210 k = 1, nmax
                        iadmoc = iszon(jiszon + ibiadm - 1 +2*k-1)
                        iadyoc = iszon(jiszon + ibiadm - 1 +2*k )
                        if (iadyoc .ne. 0) then
                            idm = iadmoc - 4
                            isd = iszon(jiszon + idm + 3) / isstat
                            isf = iszon(jiszon + iszon(jiszon+idm) - 4 ) / isstat
                            if (isd .eq. 1) then
!
!     LE SEGMENT DE VALEURS EST MARQUE X A OU X D, ON PEUT LE LIBERER
!
                                if (ixlono .ne. 0) then
                                    iblono = iadm ( jiadm(ic) + 2* ixlono-1 )
                                    lonoi = iszon(jiszon + iblono - 1 + k)
                                else
                                    lonoi = lono(jlono(ic)+ ixdeso)
                                endif
                                ltypi = ltyp( jltyp(ic)+ixdeso )
                                lsv = lonoi * ltypi
                                if (isf .eq. 4) then
                                    if (imode .eq. 2 .or. imode .eq. 3) then
!
!     ON NE TRAITE PAS LE SEGMENT DE VALEURS MARQUE X D
!
                                        goto 210
                                    endif
!
!     LE SEGMENT DE VALEURS EST MARQUE X D, IL FAUT D'ABORD L'ECRIRE
!
                                    iaddi(1) = iszon( jiszon + ibiadd - 1 + 2*k-1)
                                    iaddi(2) = iszon(jiszon + ibiadd - 1 + 2*k)
                                    call jxecro(ic, iadmoc, iaddi, lsv, j,&
                                                k)
                                    iszon(jiszon + ibiadd -1 + 2*k-1)&
                                    = iaddi(1)
                                    iszon(jiszon + ibiadd -1 + 2*k&
                                    ) = iaddi(2)
                                endif
                                lgs = iszon(jiszon+iadmoc-4) - iadmoc + 4
                                mcdyn = mcdyn - lgs
                                mldyn = mldyn + lgs
                                call hpdeallc(iadyoc, nbfree)
                                ltot = ltot + lgs
!       WRITE(6,*) ' OC ',NOM32,' OBJET ',K,' LG =',LSV,LGS,LTOT
                                iszon(jiszon + ibiadm - 1 +2*k-1) = 0
                                iszon(jiszon + ibiadm - 1 +2*k&
                                ) = 0
                                if (lmin .gt. 0) then
                                    if (ltot .ge. lmin) then
                                        lgio(1) = lgio(1)+1024*longbl( ic)*lois* (nbacce(2*ic-1)-&
                                                  & nbioav(1))
                                        lgio(2) = lgio(2)+1024*longbl( ic)*lois* (nbacce(2*ic )-n&
                                                  &bioav(2))
                                        goto 300
                                    endif
                                endif
                            endif
                        endif
210                  continue
                endif
                goto 205
!          ELSE IF ( NOM32(25:32) .EQ. ' ' ) THEN
            else
                if (iadyn .ne. 0) then
                    idm = iadmi - 4
                    isd = iszon(jiszon + idm + 3) / isstat
                    isf = iszon(jiszon + iszon(jiszon+idm) - 4) / isstat
                    if (isd .eq. 1) then
!
!     LE SEGMENT DE VALEURS EST MARQUE X A OU X D, ON PEUT LE LIBERER
!
                        ltypi = ltyp( jltyp(ic)+j )
                        lsv = lono( jlono(ic)+j ) * ltypi
                        if (isf .eq. 4) then
                            if (imode .eq. 2 .or. imode .eq. 3) then
!
!     ON NE TRAITE PAS LE SEGMENT DE VALEURS MARQUE X D
!
                                goto 205
                            endif
!
!     LE SEGMENT DE VALEURS EST MARQUE X D, IL FAUT D'ABORD L'ECRIRE
!
                            iaddi(1) = iadd ( jiadd(ic)+2*j-1 )
                            iaddi(2) = iadd ( jiadd(ic)+2*j )
                            call jxecro(ic, iadmi, iaddi, lsv, 0,&
                                        j)
                            iadd( jiadd(ic)+2*j-1 ) = iaddi(1)
                            iadd( jiadd(ic)+2*j ) = iaddi(2)
                        endif
                        lgs = iszon(jiszon+iadmi-4) - iadmi + 4
                        mcdyn = mcdyn - lgs
                        mldyn = mldyn + lgs
                        call hpdeallc(iadyn, nbfree)
                        ltot = ltot + lgs
!            write(6,*) ' OS ',NOM32,' lg =',LSV,LGS,LTOT
                        iadm(jiadm(ic)+2*j-1) = 0
                        iadm(jiadm(ic)+2*j ) = 0
                        if (lmin .gt. 0) then
                            if (ltot .ge. lmin) then
                                lgio(1) = lgio(1)+1024*longbl(ic)* lois* (nbacce(2*ic-1)-nbioav(1&
                                          &))
                                lgio(2) = lgio(2)+1024*longbl(ic)* lois* (nbacce(2*ic )-nbioav(2)&
                                          &)
                                goto 300
                            endif
                        endif
                    endif
                endif
            endif
205      continue
!
        lgio(1)=lgio(1)+1024*longbl(ic)*lois*(nbacce(2*ic-1)-nbioav(1)&
        )
        lgio(2)=lgio(2)+1024*longbl(ic)*lois*(nbacce(2*ic )-nbioav(2))
200  end do
300  continue
    mxltot=mxltot+(ltot*lois)/(1024*1024)
!
    if (lmin .ne. -2) then
!
!   ON TESTE LA VALEUR DE VMSIZE PAR RAPPORT AUX ALLOCATIONS JEVEUX ET
!   AU RELIQUAT UNIQUEMENT SI LMIN DIFFERENT DE  -2
!
        call utgtme(5, nomk, valp, iret)
        v0=valp(5)
        if (valp(3)-(valp(1)+valp(2)) .gt. 0) then
!
!   ON AJUSTE LA VALEUR DU RELIQUAT ET LA LIMITE DES ALLOCATONS JEVEUX
!
            call utptme(1, 'RLQ_MEM ', (valp(3)-valp(1)), iret)
            vx(1)=valp(4)-(valp(3)-valp(1))
            if (vx(1) .gt. 0) then
                call jermxd(vx(1)*1024*1024, iret)
                if (iret .eq. 0) then
                    call utgtme(5, nomk, valp, iret2)
!
!   ON IMPRIME UN MESSAGE D'INFORMATION SI LA VALEUR DE LIMIT_JV VARIE
!   DE PLUS DE 10 POUR CENT
!
                    if (abs(valp(5)-v0) .gt. v0*0.1d0) then
                        vx(1)=valp(2)
                        vx(2)=valp(5)
                        call u2mesr('I', 'JEVEUX1_74', 2, vx)
                    endif
                endif
            endif
        endif
    endif
!
    call uttcpu('CPU.MEMD.1', 'FIN', ' ')
!
end subroutine
