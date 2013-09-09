subroutine aceaco(nomu, noma, lmax, locagb, locamb,&
                  nbocc)
    implicit none
#include "jeveux.h"
#include "asterc/r8pi.h"
#include "asterfort/alcart.h"
#include "asterfort/angvx.h"
#include "asterfort/assert.h"
#include "asterfort/exisd.h"
#include "asterfort/getvem.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nocart.h"
#include "asterfort/wkvect.h"
!
    integer :: lmax, nbocc
    logical :: locagb, locamb
    character(len=8) :: nomu, noma
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ----------------------------------------------------------------------
!                          AFFE_CARA_ELEM
!
!     AFFECTATION DES CARACTERISTIQUES POUR L'ELEMENT COQUE
!
! ----------------------------------------------------------------------
!  IN
!     NOMU   : NOM UTILISATEUR DE LA COMMANDE
!     NOMA   : NOM DU MAILLAGE
!     LMAX   : NOMBRE MAXIMUM DE MAILLE AFFECTEES PAR AFFE_CARA_ELEM
!     LOCAGB : EXISTANCE DE GRILLE
!     LOCAMB : EXISTANCE DE MEMBRANE
!     NBOCC  : NOMBRE D'OCCURENCES DU MOT CLE COQUE
! ----------------------------------------------------------------------
    integer :: nvec, iarg, i, ioc, jdcc, jdls, jdvc, jdccf, jdvcf, jdls2
    integer :: na, nco, ncr, nex, ng, nin, nk, nm, nv, nvf, nexf
    integer :: iret
    logical :: lcartf
    real(kind=8) :: ang(2), epa, kappa, correc, rigi, excent
    real(kind=8) :: vect(3), pi, xiner
    character(len=8) :: inert, korrec, epaf, excf
    character(len=19) :: cartco, cartcf
    character(len=24) :: tmpnco, tmpvco, tmpncf, tmpvcf
!-----------------------------------------------------------------------
    pi=r8pi()
!
! --- CONSTRUCTION DES CARTES ET ALLOCATION
    call jemarq()
!
!     CARTE POUR LES VALEURS REELLES
    cartco = nomu//'.CARCOQUE'
    call exisd('CARTE', cartco, iret)
    if (iret .eq. 0) then
        call alcart('G', cartco, noma, 'CACOQU')
    endif
    tmpnco = cartco//'.NCMP'
    tmpvco = cartco//'.VALV'
    call jeveuo(tmpnco, 'E', jdcc)
    call jeveuo(tmpvco, 'E', jdvc)
!     LES NOMS DES GRANDEURS REELLES
    zk8(jdcc) = 'EP'
    zk8(jdcc+1) = 'ALPHA'
    zk8(jdcc+2) = 'BETA'
    zk8(jdcc+3) = 'KAPPA'
    zk8(jdcc+4) = 'C_METR'
    zk8(jdcc+5) = 'CTOR'
    zk8(jdcc+6) = 'EXCENT'
    zk8(jdcc+7) = 'INERTIE'
!
!     CARTE POUR LES FONCTIONS
    cartcf = nomu//'.CARCOQUF'
    call exisd('CARTE', cartcf, iret)
    lcartf = .false.
    if (iret .eq. 0) then
! ------ DOIT-ON CREER LA CARTE DE FONCTION
        do 100 ioc = 1, nbocc
            call getvid('COQUE', 'EPAIS_FO', iocc=ioc, scal=epaf, nbret=nvf)
            call getvid('COQUE', 'EXCENTREMENT_FO', iocc=ioc, scal=excf, nbret=nexf)
            if (nvf+nexf .ne. 0) then
                lcartf = .true.
                goto 110
            endif
100      continue
110      continue
!
!        CARTE POUR LES NOMS DES FONCTIONS
        if (lcartf) then
            call alcart('V', cartcf, noma, 'CACOQUF')
        endif
    else
        lcartf = .true.
    endif
!     SI LA CARTE EXISTE
    if (lcartf) then
        tmpncf = cartcf//'.NCMP'
        tmpvcf = cartcf//'.VALV'
        call jeveuo(tmpncf, 'E', jdccf)
        call jeveuo(tmpvcf, 'E', jdvcf)
!        LES NOMS DES FONCTIONS
        zk8(jdccf) = 'EP'
        zk8(jdccf+1)= 'EXCENT'
    endif
!
    call wkvect('&&TMPCOQUE', 'V V K24', lmax, jdls)
    call wkvect('&&TMPCOQUE2', 'V V K8', lmax, jdls2)
!
! --- LECTURE DES VALEURS ET AFFECTATION DANS : CARTCO OU CARTCF
    do 10 ioc = 1, nbocc
        ang(1) = 0.d0
        ang(2) = 0.d0
        correc = 0.d0
        kappa = 0.d0
        excent = 0.d0
        xiner = 0.d0
        inert = 'NON'
        call getvem(noma, 'GROUP_MA', 'COQUE', 'GROUP_MA', ioc,&
                    iarg, lmax, zk24(jdls), ng)
        call getvem(noma, 'MAILLE', 'COQUE', 'MAILLE', ioc,&
                    iarg, lmax, zk8(jdls2), nm)
        call getvr8('COQUE', 'EPAIS', iocc=ioc, scal=epa, nbret=nv)
        call getvid('COQUE', 'EPAIS_FO', iocc=ioc, scal=epaf, nbret=nvf)
        call getvr8('COQUE', 'ANGL_REP', iocc=ioc, nbval=2, vect=ang,&
                    nbret=na)
        call getvr8('COQUE', 'VECTEUR', iocc=ioc, nbval=3, vect=vect,&
                    nbret=nvec)
        call getvr8('COQUE', 'A_CIS', iocc=ioc, scal=kappa, nbret=nk)
        call getvtx('COQUE', 'MODI_METRIQUE', iocc=ioc, scal=korrec, nbret=nco)
        call getvr8('COQUE', 'COEF_RIGI_DRZ', iocc=ioc, scal=rigi, nbret=ncr)
        call getvr8('COQUE', 'EXCENTREMENT', iocc=ioc, scal=excent, nbret=nex)
        call getvid('COQUE', 'EXCENTREMENT_FO', iocc=ioc, scal=excf, nbret=nexf)
        call getvtx('COQUE', 'INER_ROTA', iocc=ioc, scal=inert, nbret=nin)
!        EPAIS EST OBLIGATOIRE : ASSERT SI PAS LA
        if (nv .ne. 0) then
            zr(jdvc) = epa
            if (lcartf) zk8(jdvcf) = '&&ACEACO'
        else if (nvf .ne. 0) then
            zr(jdvc) = 0.0d0
            if (lcartf) zk8(jdvcf) = epaf
        else
            ASSERT(.false.)
        endif
        zr(jdvc+1) = ang(1)
        zr(jdvc+2) = ang(2)
        if (nvec .ne. 0) then
            call angvx(vect, ang(1), ang(2))
            zr(jdvc+1) = ang(1)*180.d0/pi
            zr(jdvc+2) = ang(2)*180.d0/pi
        endif
        zr(jdvc+3) = kappa
        if (korrec .eq. 'OUI') correc=1.d0
        zr(jdvc+4) = correc
        zr(jdvc+5) = rigi
!
        zr(jdvc+6) = excent
        if (lcartf) zk8(jdvcf+1)= '&&ACEACO'
        if (nexf .ne. 0) then
            zr(jdvc+6) = 0.0d0
            if (lcartf) zk8(jdvcf+1)= excf
        endif
!
        if ((nex+nexf) .ne. 0 .and. nin .eq. 0) inert = 'OUI'
        if (inert .eq. 'OUI') xiner=1.d0
        zr(jdvc+7) = xiner
!
! ---    "GROUP_MA" = TOUTES LES MAILLES DE LA LISTE DE GROUPES MAILLES
        if (ng .gt. 0) then
            do 20 i = 1, ng
                call nocart(cartco, 2, 8, groupma=zk24(jdls+i-1))
20          continue
            if (lcartf) then
                do 25 i = 1, ng
                    call nocart(cartcf, 2, 2, groupma=zk24(jdls+i-1))
25              continue
            endif
        endif
!
! ---    "MAILLE" = TOUTES LES MAILLES DE LA LISTE DE MAILLES
        if (nm .gt. 0) then
            call nocart(cartco, 3, 8, mode='NOM', nma=nm,&
                        limano=zk8(jdls2))
            if (lcartf) then
                call nocart(cartcf, 3, 2, mode='NOM', nma=nm,&
                            limano=zk8(jdls2))
            endif
        endif
!
10  end do
!
    call jedetr('&&TMPCOQUE')
    call jedetr('&&TMPCOQUE2')
!     SI NI GRILLE NI MEMBRANE
    if ((.not.locagb) .and. (.not.locamb)) then
        call jedetr(tmpnco)
        call jedetr(tmpvco)
        if (lcartf) then
            call jedetr(tmpncf)
            call jedetr(tmpvcf)
        endif
    endif
!
    call jedema()
end subroutine
