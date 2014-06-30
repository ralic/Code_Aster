subroutine avcrit(nbvec, nbordr, vectn, vwork, tdisp,&
                  kwork, sommw, tspaq, i, vala,&
                  coefpa, ncycl, jvmin, jvmax, jomin,&
                  jomax, nomcri, nomfor, jgdreq)
!
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
! person_in_charge: van-xuan.tran at edf.fr
    implicit none
#include "jeveux.h"
#include "asterfort/anacri.h"
#include "asterfort/aveppr.h"
#include "asterfort/aveteq.h"
#include "asterfort/avetpr.h"
#include "asterfort/avphyd.h"
#include "asterfort/avsieq.h"
#include "asterfort/avsign.h"
#include "asterfort/avsipr.h"
#include "asterfort/fointe.h"
#include "asterfort/fonbpa.h"
#include "asterfort/getvr8.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
!
    integer :: nbvec, nbordr, ncycl(nbvec)
    integer :: tdisp, kwork, sommw, tspaq, i
    real(kind=8) :: vectn(3*nbvec)
    real(kind=8) :: vwork(tdisp)
    real(kind=8) :: vala, coefpa
    integer :: jomin, jomax, jvmin, jvmax, jgdreq
    character(len=16) :: nomcri, nomfor
! ----------------------------------------------------------------------
! BUT: CALCULER LA CONTRAINTE EQUIVALENTE POUR TOUS LES VECTEURS NORMAUX
!      A TOUS LES NUMEROS D'ORDRE.
! ----------------------------------------------------------------------
! ARGUMENTS :
!  NBVEC    IN   I  : NOMBRE DE VECTEURS NORMAUX.
!  NBORDR   IN   I  : NOMBRE DE NUMEROS D'ORDRE.
!  VALA     IN   R  : VALEUR DU PARAMETRE a ASSOCIE AU CRITERE.
!  COEFPA   IN   R  : COEFFICIENT DE PASSAGE CISAILLEMENT - UNIAXIAL.
!  NCYCL    IN   I  : NOMBRE DE CYCLES ELEMENTAIRES POUR TOUS LES
!                     VECTEURS NORMAUX.
! JVMIN      IN  I  : ADDRESEE JEUVEUX DES VALEURS MIN DES CYCLES ELEMENTAIRES
!                     POUR TOUS LES VECTEURS NORMAUX.
! JVMAX      IN  I  : ADDRESEE JEUVEUX DES VALEURS MAX DES CYCLES ELEMENTAIRES
!                     POUR TOUS LES VECTEURS NORMAUX.
! JOMIN      IN  I  : ADDRESEE JEUVEUX DES NUMEROS D'ORDRE ASSOCIES AUX VALEURS
!                     MIN DESCYCLES ELEMENTAIRES POUR TOUS LES VECTEURS
!                     NORMAUX.
! JOMAX      IN  I  : ADDRESEE JEUVEUX DES NUMEROS D'ORDRE ASSOCIES AUX VALEURS
!
! JGDREQ    OUT  I  : ADDRESSE VECTEUR CONTENANT LES VALEURS DE LA GRANDEUR
!                     EQUIVALENTE, POUR TOUS LES NUMEROS D'ORDRE
!                     DE CHAQUE VECTEUR NORMAL.
! ----------------------------------------------------------------------
    integer :: ivect, ad0, ad1, ad2, icycl, nval, ipar, j, np
    integer :: ibid, nparma, jprof, paract(35)
    real(kind=8) :: coepre, valpar(30), valpu(30)
    integer :: jvsign, jvphyd, jvsipr, jvepsn, jvetpr, jvsitn, jveppr
    integer :: jvsipn, jvsieq, jveteq
!
    logical(kind=1) :: lbid
    character(len=8) :: nompf(30), nompar(30)
    character(len=16) :: typcha
    character(len=24) :: chnom, cbid
!  VSIGN    IN   R  : VECTEUR CONTENANT LES VALEURS DE LA CONTRAINTE
!                     NORMALE, POUR TOUS LES NUMEROS D'ORDRE
!                     DE CHAQUE VECTEUR NORMAL, ON UTILISE
!                     VSIGN UNIQUEMENT DANS LE CRITERE MATAKE_MODI_AV.
!  VPHYDR   IN   R  : VECTEUR CONTENANT LA PRESSION HYDROSTATIQUE A
!                     TOUS LES INSTANTS, ON UTILISE VPHYDR
!                     UNIQUEMENT DANS LE CRITERE DE DANG VAN.
!     ------------------------------------------------------------------
    data  nompar /   'TAUPR_1','TAUPR_2','SIGN_1',  'SIGN_2',&
     &                 'PHYDR_1','PHYDR_2','EPSPR_1', 'EPSPR_2',&
     &                 'SIPR1_1','SIPR1_2','EPSN1_1', 'EPSN1_2',&
     &                 'ETPR1_1','ETPR1_2','SITN1_1', 'SITN1_2',&
     &                 'EPPR1_1','EPPR1_2','SIPN1_1', 'SIPN1_2',&
     &                 'SIGEQ_1','SIGEQ_2', 'ETEQ_1', 'ETEQ_2',&
     &                 'EPEQ_1', 'EPEQ_2',  'INVJ2_1','INVJ2_2',&
     &                 'SITRE_1','SITRE_2'     /
!-----------------------------------------------------------------------
!
!234567                                                              012
!
    call jemarq()
!
!
    call wkvect('&&AVCRIT.VSIGN', 'V V R', nbvec*nbordr, jvsign)
    call wkvect('&&AVCRIT.VPHYDR', 'V V R', nbordr, jvphyd)
    call wkvect('&&AVCRIT.VSIPR', 'V V R', nbordr, jvsipr)
    call wkvect('&&AVCRIT.VEPSN', 'V V R', nbordr, jvepsn)
    call wkvect('&&AVCRIT.VETPR', 'V V R', nbordr, jvetpr)
    call wkvect('&&AVCRIT.VSITN', 'V V R', nbordr, jvsitn)
    call wkvect('&&AVCRIT.VEPPR', 'V V R', nbordr, jveppr)
    call wkvect('&&AVCRIT.VSIPN', 'V V R', nbordr, jvsipn)
    call wkvect('&&AVCRIT.VSIEQ', 'V V R', nbordr, jvsieq)
    call wkvect('&&AVCRIT.VETEQ', 'V V R', nbordr, jveteq)
!
! RECUPERER LA LISTE DE GRANDEURS ACTIVES
!
    typcha = 'NON_PERIODIQUE'
!
    call anacri(nomcri, nomfor, typcha, 'NON', paract,&
                lbid, lbid, lbid, lbid, lbid)
!
!-----------------------------------------------------------------------
! CALCULER LES GRANDEURS
!----------------------------------------------------------------------
! 1.1 CALCUL DE LA CONTRAINTE NORMALE
    if ((paract(3) .eq. 1 ) .or. (paract(4) .eq. 1 )) then
        call avsign(nbvec, nbordr, vectn, vwork, tdisp,&
                    kwork, sommw, tspaq, i, jvsign)
!
    endif
!
! 1.2 CALCUL DE LA PRESSION HYDROSTATIQUE
    if ((paract(5) .eq. 1 ) .or. (paract(6) .eq. 1 )) then
!
        call avphyd(nbordr, vwork, tdisp, kwork, sommw,&
                    tspaq, i, jvphyd)
!
    endif
!
    if ((paract(9) .eq. 1 ) .or. (paract(10) .eq. 1 ) .or. (paract(11) .eq. 1 ) .or.&
        (paract(12) .eq. 1 )) then
!
        call avsipr(nbordr, vwork, tdisp, kwork, sommw,&
                    tspaq, i, jvsipr, jvepsn)
!
    endif
!
!
    if ((paract(13) .eq. 1 ) .or. (paract(14) .eq. 1 ) .or. (paract(15) .eq. 1 ) .or.&
        (paract(16) .eq. 1 )) then
!
        call avetpr(nbordr, vwork, tdisp, kwork, sommw,&
                    tspaq, i, jvetpr, jvsitn)
!
    endif
!
    if ((paract(17) .eq. 1 ) .or. (paract(18) .eq. 1 ) .or. (paract(19) .eq. 1 ) .or.&
        (paract(20) .eq. 1 )) then
!
        call aveppr(nbordr, vwork, tdisp, kwork, sommw,&
                    tspaq, i, jveppr, jvsipn)
!
    endif
!
    if ((paract(21) .eq. 1 ) .or. (paract(22) .eq. 1 )) then
!
        call avsieq(nbordr, vwork, tdisp, kwork, sommw,&
                    tspaq, i, jvsieq)
!
    endif
!
    if ((paract(23) .eq. 1 ) .or. (paract(24) .eq. 1 )) then
!
        call aveteq(nbordr, vwork, tdisp, kwork, sommw,&
                    tspaq, i, jveteq)
!
    endif
!----------------------------------------------------------------------
! EVALUER LES GRANDEURS
!----------------------------------------------------------------------
! 1. CRITERE DE DANG_VAN MODIFIE (AMPLITUDE VARIABLE)
!      IF (.NOT. POST) THEN
    call getvr8(' ', 'COEF_PREECROU', scal=coepre, nbret=nval)
!      ENDIF
!
    if (nomcri(1:7) .eq. 'FORMULE') then
! NOMBRE DE PARAMETRES DISPONIBLES
        nparma = 30
! RECUPERER LES NOMS DE PARAMETRES FOURNIS PAR L'UTILISATEUR
        chnom(20:24) = '.PROL'
        chnom(1:19) = nomfor
!
        call jeveuo(chnom, 'L', jprof)
        call fonbpa(nomfor, zk24(jprof), cbid, nparma, np,&
                    nompf)
    endif
!
    do ivect = 1, nbvec
        ad0 = (ivect-1)*nbordr
        do icycl = 1, ncycl(ivect)
            ad1 = (ivect-1)*nbordr + icycl
            ad2 = (ivect-1)*(nbordr+2) + icycl
!
            if (nomcri(1:14) .eq. 'MATAKE_MODI_AV') then
                zr(jgdreq+ad1)= coepre*abs((zr(jvmax+ad2) - zr(jvmin+ad2))/2.0d0)&
                + vala*max(zr(jvsign+ad0+zi(jomax+ad2)), &
                zr(jvsign+ad0+zi(jomin+ad2)), 0.0d0)
                zr(jgdreq+ad1)= zr(jgdreq+ad1)*coefpa
!
            endif
!
            if (nomcri(1:16) .eq. 'DANG_VAN_MODI_AV') then
                zr(jgdreq+ad1)= coepre*abs((zr(jvmax+ad2) - zr(jvmin+ad2))/2.0d0)&
                + vala*max(zr(jvphyd+zi(jomax+ad2)), &
                zr(jvphyd+zi(jomin+ad2)),0.0d0)
!
                zr(jgdreq+ad1)= zr(jgdreq+ad1)*coefpa
            endif
!
!  Pour le critère de FATEMI_SOCIE? il faut utiliser le tenseur de déformation du 
!  type d'ingéneire gamma_ij = 2*eps_ij avec i #j

            if (nomcri(1:16) .eq. 'FATESOCI_MODI_AV') then
                zr(jgdreq+ad1)= coepre*abs((zr(jvmax+ad2) - zr(jvmin+ad2))/2.0d0*2.0d0)&
                *(1.0d0 + vala*max(zr(jvsign+ad0+zi(jomax+ad2)), &
                zr(jvsign+ad0+zi(jomin+ad2)), 0.0d0))
                zr(jgdreq+ad1)= zr(jgdreq+ad1)*coefpa
            endif
!
            if (nomcri(1:7) .eq. 'FORMULE') then
                valpar(1) = zr(jvmax+ad2)
                valpar(2) = zr(jvmin+ad2)
                valpar(3) = zr(jvsign+ad0+zi(jomax+ad2))
                valpar(4) = zr(jvsign+ad0+zi(jomin+ad2))
                valpar(5) = zr(jvphyd+zi(jomax+ad2))
                valpar(6) = zr(jvphyd+zi(jomin+ad2))
                valpar(7) = zr(jvmax+ad2)
                valpar(8) = zr(jvmin+ad2)
                valpar(9) = zr(jvsipr+zi(jomax+ad2))
                valpar(10) = zr(jvsipr+zi(jomin+ad2))
                valpar(11) = zr(jvepsn+zi(jomax+ad2))
                valpar(12) = zr(jvepsn+zi(jomin+ad2))
                valpar(13) = zr(jvetpr+zi(jomax+ad2))
                valpar(14) = zr(jvetpr+zi(jomin+ad2))
                valpar(15) = zr(jvsitn+zi(jomax+ad2))
                valpar(16) = zr(jvsitn+zi(jomin+ad2))
                valpar(17) = zr(jveppr+zi(jomax+ad2))
                valpar(18) = zr(jveppr+zi(jomin+ad2))
                valpar(19) = zr(jvsipn+zi(jomax+ad2))
                valpar(20) = zr(jvsipn+zi(jomin+ad2))
                valpar(21) = zr(jvsieq+zi(jomax+ad2))
                valpar(22) = zr(jvsieq+zi(jomin+ad2))
                valpar(23) = zr(jveteq+zi(jomax+ad2))
                valpar(24) = zr(jveteq+zi(jomin+ad2))
                valpar(25) = 0.d0
                valpar(26) = 0.d0
                valpar(27) = 0.d0
                valpar(28) = 0.d0
                valpar(29) = 0.d0
                valpar(30) = 0.d0
!
                do 75 j = 1, np
                    do ipar = 1, nparma
                        if (nompf(j) .eq. nompar(ipar)) then
                            valpu(j) = valpar(ipar)
                            goto 75
                        endif
                    end do
75              continue
!
                call fointe('F', nomfor, np, nompf, valpu,&
                            zr(jgdreq+ad1), ibid)
!
            endif
!
        end do
!
    end do
!
    call jedetr('&&AVCRIT.VSIGN')
    call jedetr('&&AVCRIT.VPHYDR')
    call jedetr('&&AVCRIT.VSIPR')
    call jedetr('&&AVCRIT.VEPSN')
    call jedetr('&&AVCRIT.VETPR')
    call jedetr('&&AVCRIT.VSITN')
    call jedetr('&&AVCRIT.VEPPR')
    call jedetr('&&AVCRIT.VSIPN')
    call jedetr('&&AVCRIT.VSIEQ')
    call jedetr('&&AVCRIT.VETEQ')
!
    call jedema()
!
end subroutine
