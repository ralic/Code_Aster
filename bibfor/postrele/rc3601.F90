subroutine rc3601(ig, iocs, seisme, npass, ima,&
                  ipt, nbm, adrm, c, k,&
                  cara, nommat, snmax, samax, utot,&
                  sm, factus)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/rc36fp.h"
#include "asterfort/rc36fs.h"
#include "asterfort/rc36fu.h"
#include "asterfort/rc36sa.h"
#include "asterfort/rc36sn.h"
#include "asterfort/rc36sp.h"
#include "asterfort/rcma01.h"
#include "asterfort/rcmo01.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    integer :: ig, iocs, npass, ima, ipt, nbm, adrm(*)
    real(kind=8) :: c(*), k(*), cara(*), snmax, samax, utot, sm, factus(*)
    aster_logical :: seisme
    character(len=8) :: nommat
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3600
!
!     CALCUL DES AMPLITUDES DE CONTRAINTES
!     CALCUL DU FACTEUR D'USAGE
!     ON TRAITE LES SITUATIONS COMBINABLES DANS LEUR GROUPE
!
!     Soit 2 états stabilisés I et J appartenant aux situations P et Q
!
!     on calcule le SALT(I,J) = 0,5*(EC/E)*Ke*Sn(P,Q)*Sp(I,J)
!
!     avec Sn(P,Q) = Max( Sn(I,J) )
!          Sn(I,J) = Max( Max(Sn(I,J,ThP)), Max(Sn(I,J,ThQ)) )
!
!     avec Sp(I,J) = Max( Max(Sp(I,J,ThP)), Max(Sp(I,J,ThQ)) )
!
!     ------------------------------------------------------------------
!
    integer :: nbsigr, jnsg, is1, ioc1, is2, ioc2, inds, ifm, niv, jcombi
    integer :: nbth1, jth1, nbth2, jth2
    integer :: ndim, nscy, ns, nbsig2, i1, i2, indi
    integer :: nbsitu
    real(kind=8) :: ppi, ppj, pqi, pqj, saltij, ug, sn, sp, smm, mpi(3), mpj(3)
    real(kind=8) :: mqi(3), mqj(3), mse(3), matpi(14), matpj(14), matqi(14)
    real(kind=8) :: matqj(14), matse(14)
    character(len=24) :: momepi, momepj, momeqi, momeqj, matepi, matepj, mateqi
    character(len=24) :: mateqj
    real(kind=8) :: typeke, spmeca, spther
    integer, pointer :: situ_numero(:) => null()
    integer, pointer :: impr_situ(:) => null()
    real(kind=8), pointer :: matrice_salt(:) => null()
    real(kind=8), pointer :: matrice_sn(:) => null()
    integer, pointer :: nb_occurr(:) => null()
    integer, pointer :: situ_nb_occur(:) => null()
    real(kind=8), pointer :: situ_pres_b(:) => null()
    character(len=24), pointer :: situ_moment_b(:) => null()
    character(len=24), pointer :: situ_moment_a(:) => null()
    real(kind=8), pointer :: situ_pres_a(:) => null()
    character(len=24), pointer :: materiau(:) => null()
! DEB ------------------------------------------------------------------
    call jemarq()
!
    call infniv(ifm, niv)
!
    call jeveuo('&&RC3600.SITU_NUMERO', 'L', vi=situ_numero)
    call jeveuo('&&RC3600.SITU_COMBINABLE', 'L', jcombi)
    call jeveuo('&&RC3600.SITU_PRES_A', 'L', vr=situ_pres_a)
    call jeveuo('&&RC3600.SITU_PRES_B', 'L', vr=situ_pres_b)
    call jeveuo('&&RC3600.SITU_MOMENT_A', 'L', vk24=situ_moment_a)
    call jeveuo('&&RC3600.SITU_MOMENT_B', 'L', vk24=situ_moment_b)
    call jeveuo('&&RC3600.SITU_NB_OCCUR', 'L', vi=situ_nb_occur)
!
    call jeveuo('&&RC3600.MATERIAU', 'L', vk24=materiau)
!
    call jelira('&&RC3600.SITU_PRES_A', 'LONUTI', nbsitu)
    call jelira(jexnum('&&RC3600.LES_GROUPES', ig), 'LONMAX', nbsigr)
    call jeveuo(jexnum('&&RC3600.LES_GROUPES', ig), 'L', jnsg)
    if (niv .ge. 2) then
        write (ifm,1000) ig,nbsigr
        write (ifm,1002) (situ_numero(1+zi(jnsg+i1-1)-1),i1=1,nbsigr)
    endif
!
    if (iocs .eq. 0) then
        nbsig2 = nbsigr
    else
        nbsig2 = nbsigr - 1
    endif
    ndim = 2*nbsig2
    AS_ALLOCATE(vi=nb_occurr, size=ndim)
    AS_ALLOCATE(vi=impr_situ, size=ndim)
    ndim = nbsig2*nbsig2
    AS_ALLOCATE(vr=matrice_sn, size=ndim)
    ndim = 4*nbsig2*nbsig2
    AS_ALLOCATE(vr=matrice_salt, size=ndim)
!
    ns = 0
    if (seisme) then
        momepi = situ_moment_a(1+nbsitu+iocs-1)
        call rcmo01(momepi, ima, ipt, mse)
        mse(1) = 2*mse(1)
        mse(2) = 2*mse(2)
        mse(3) = 2*mse(3)
        matepi = materiau(1+2*(nbsitu+iocs)-1)
        call rcma01(matepi, ima, ipt, nbm, adrm,&
                    matse)
        ns = situ_nb_occur(1+2*(nbsitu+iocs)-2)
        nscy = situ_nb_occur(1+2*(nbsitu+iocs)-1)
    else
        mse(1) = 0.d0
        mse(2) = 0.d0
        mse(3) = 0.d0
    endif
!
!
! --- SITUATION P :
!     -------------
!
    i1 = 0
    do 20 is1 = 1, nbsigr
        ioc1 = zi(jnsg+is1-1)
        if (.not.zl(jcombi+ioc1-1)) goto 20
        if (ioc1 .gt. nbsitu) goto 20
!
        i1 = i1 + 1
        nb_occurr(2* (i1-1)+1) = situ_nb_occur(1+2*ioc1-2)
        nb_occurr(2* (i1-1)+2) = situ_nb_occur(1+2*ioc1-2)
        impr_situ(2* (i1-1)+1) = ioc1
        impr_situ(2* (i1-1)+2) = ioc1
!
        ppi = situ_pres_a(ioc1)
        momepi = situ_moment_a(ioc1)
        call rcmo01(momepi, ima, ipt, mpi)
        matepi = materiau(1+2*ioc1-1)
        call rcma01(matepi, ima, ipt, nbm, adrm,&
                    matpi)
        typeke = matpi(14)
!
        ppj = situ_pres_b(ioc1)
        momepj = situ_moment_b(ioc1)
        call rcmo01(momepj, ima, ipt, mpj)
        matepj = materiau(1+2*ioc1-2)
        call rcma01(matepj, ima, ipt, nbm, adrm,&
                    matpj)
!
        call jelira(jexnum('&&RC3600.SITU_THERMIQUE', ioc1), 'LONUTI', nbth1)
        if (nbth1 .ne. 0) then
            call jeveuo(jexnum('&&RC3600.SITU_THERMIQUE', ioc1), 'L', jth1)
        else
            jth1 = 1
        endif
!
        nbth2 = 0
        jth2 = 1
        ioc2 = 0
!
        sn = 0.d0
        call rc36sn(nbm, adrm, ipt, c, cara,&
                    matpi, ppi, mpi, matpj, ppj,&
                    mpj, mse, nbth1, nbth2, ioc1,&
                    ioc2, sn)
        matrice_sn(nbsig2* (i1-1)+i1) = sn
        snmax = max(snmax,sn)
!
        if (niv .ge. 2) write (ifm,1010) ioc1,sn
        inds = 4*nbsig2* (i1-1) + 4* (i1-1)
!
!
! ----- 1/ CALCUL DU SALT(I,I) A PARTIR DU SN(P,Q) ET SP(I,I)
!
        saltij = 0.d0
        matrice_salt(inds+1) = saltij
!
! ----- 2/ CALCUL DU SALT(I,J) A PARTIR DU SN(P,Q) ET SP(I,J)
!
        sp = 0.d0
        spmeca = 0.d0
        spther = 0.d0
        call rc36sp(nbm, adrm, ipt, c, k,&
                    cara, matpi, ppi, mpi, matpj,&
                    ppj, mpj, mse, nbth1, nbth2,&
                    ioc1, ioc2, sp, typeke, spmeca,&
                    spther)
!
        if (niv .ge. 2) write (ifm,1032) sp
        if (typeke .gt. 0.d0) then
            if (niv .ge. 2) then
                write (ifm,1132) spmeca,spther
            endif
        endif
!
        call rc36sa(nommat, matpi, matpj, sn, sp,&
                    typeke, spmeca, spther, saltij, smm)
!
        matrice_salt(inds+2) = saltij
        matrice_salt(inds+3) = saltij
        if (saltij .gt. samax) then
            samax = saltij
            sm = smm
        endif
!
! ----- 3/ CALCUL DU SALT(J,J) A PARTIR DU SN(P,Q) ET SP(J,J)
!
        saltij = 0.d0
        matrice_salt(inds+4) = saltij
!
! ----- SITUATION Q :
!       -------------
!
        i2 = i1
        do 10 is2 = is1 + 1, nbsigr
            ioc2 = zi(jnsg+is2-1)
            if (.not.zl(jcombi+ioc2-1)) goto 10
            if (ioc2 .gt. nbsitu) goto 10
            i2 = i2 + 1
!
            pqi = situ_pres_a(ioc2)
            momeqi = situ_moment_a(ioc2)
            call rcmo01(momeqi, ima, ipt, mqi)
            mateqi = materiau(1+2*ioc2-1)
            call rcma01(mateqi, ima, ipt, nbm, adrm,&
                        matqi)
!
            pqj = situ_pres_b(ioc2)
            momeqj = situ_moment_b(ioc2)
            call rcmo01(momeqj, ima, ipt, mqj)
            mateqj = materiau(1+2*ioc2-2)
            call rcma01(mateqj, ima, ipt, nbm, adrm,&
                        matqj)
!
            call jelira(jexnum('&&RC3600.SITU_THERMIQUE', ioc2), 'LONUTI', nbth2)
            if (nbth2 .ne. 0) then
                call jeveuo(jexnum('&&RC3600.SITU_THERMIQUE', ioc2), 'L', jth2)
            else
                jth2 = 1
            endif
!
! ------- CALCUL DU SN(P,Q), ON A 4 COMBINAISONS
!
            sn = 0.d0
!
            call rc36sn(nbm, adrm, ipt, c, cara,&
                        matpi, ppi, mpi, matqi, pqi,&
                        mqi, mse, nbth1, nbth2, ioc1,&
                        ioc2, sn)
!
            call rc36sn(nbm, adrm, ipt, c, cara,&
                        matpi, ppi, mpi, matqj, pqj,&
                        mqj, mse, nbth1, nbth2, ioc1,&
                        ioc2, sn)
!
            call rc36sn(nbm, adrm, ipt, c, cara,&
                        matpj, ppj, mpj, matqj, pqj,&
                        mqj, mse, nbth1, nbth2, ioc1,&
                        ioc2, sn)
!
            call rc36sn(nbm, adrm, ipt, c, cara,&
                        matpj, ppj, mpj, matqi, pqi,&
                        mqi, mse, nbth1, nbth2, ioc1,&
                        ioc2, sn)
!
            matrice_sn(nbsig2* (i1-1)+i2) = sn
            matrice_sn(nbsig2* (i2-1)+i1) = sn
            if (niv .ge. 2) write (ifm,1020) ioc1,ioc2,sn
!
            snmax = max(snmax,sn)
            inds = 4*nbsig2* (i1-1) + 4* (i2-1)
            indi = 4*nbsig2* (i2-1) + 4* (i1-1)
!
! ------- 1/ CALCUL DU SALT(I,I) A PARTIR DU SN(P,Q) ET SP(I,I)
!
            sp = 0.d0
            spmeca = 0.d0
            spther = 0.d0
!
            call rc36sp(nbm, adrm, ipt, c, k,&
                        cara, matpi, ppi, mpi, matqi,&
                        pqi, mqi, mse, nbth1, nbth2,&
                        ioc1, ioc2, sp, typeke, spmeca,&
                        spther)
!
            if (niv .ge. 2) write (ifm,1031) sp
            if (typeke .gt. 0.d0) then
                if (niv .ge. 2) then
                    write (ifm,1131) spmeca,spther
                endif
            endif
!
            call rc36sa(nommat, matpi, matqi, sn, sp,&
                        typeke, spmeca, spther, saltij, smm)
!
            matrice_salt(inds+1) = saltij
            matrice_salt(indi+1) = saltij
            if (saltij .gt. samax) then
                samax = saltij
                sm = smm
            endif
!
! ------- 2/ CALCUL DU SALT(I,J) A PARTIR DU SN(P,Q) ET SP(I,J)
!
            sp = 0.d0
            spmeca = 0.d0
            spther = 0.d0
!
            call rc36sp(nbm, adrm, ipt, c, k,&
                        cara, matpi, ppi, mpi, matqj,&
                        pqj, mqj, mse, nbth1, nbth2,&
                        ioc1, ioc2, sp, typeke, spmeca,&
                        spther)
!
            if (niv .ge. 2) write (ifm,1032) sp
            if (typeke .gt. 0.d0) then
                if (niv .ge. 2) then
                    write (ifm,1132) spmeca,spther
                endif
            endif
!
            call rc36sa(nommat, matpi, matqj, sn, sp,&
                        typeke, spmeca, spther, saltij, smm)
!
            matrice_salt(inds+3) = saltij
            matrice_salt(indi+2) = saltij
            if (saltij .gt. samax) then
                samax = saltij
                sm = smm
            endif
!
! ------- 3/ CALCUL DU SALT(J,I) A PARTIR DU SN(P,Q) ET SP(J,I)
!
            sp = 0.d0
            spmeca = 0.d0
            spther = 0.d0
!
            call rc36sp(nbm, adrm, ipt, c, k,&
                        cara, matpj, ppj, mpj, matqi,&
                        pqi, mqi, mse, nbth1, nbth2,&
                        ioc1, ioc2, sp, typeke, spmeca,&
                        spther)
!
            if (niv .ge. 2) write (ifm,1034) sp
            if (typeke .gt. 0.d0) then
                if (niv .ge. 2) then
                    write (ifm,1134) spmeca,spther
                endif
            endif
!
            call rc36sa(nommat, matpj, matqi, sn, sp,&
                        typeke, spmeca, spther, saltij, smm)
!
            matrice_salt(inds+2) = saltij
            matrice_salt(indi+3) = saltij
            if (saltij .gt. samax) then
                samax = saltij
                sm = smm
            endif
!
! ------- 4/ CALCUL DU SALT(J,J) A PARTIR DU SN(P,Q) ET SP(J,J)
!
            sp = 0.d0
            spmeca = 0.d0
            spther = 0.d0
!
            call rc36sp(nbm, adrm, ipt, c, k,&
                        cara, matpj, ppj, mpj, matqj,&
                        pqj, mqj, mse, nbth1, nbth2,&
                        ioc1, ioc2, sp, typeke, spmeca,&
                        spther)
!
            if (niv .ge. 2) write (ifm,1033) sp
            if (typeke .gt. 0.d0) then
                if (niv .ge. 2) then
                    write (ifm,1133) spmeca,spther
                endif
            endif
!
            call rc36sa(nommat, matpj, matqj, sn, sp,&
                        typeke, spmeca, spther, saltij, smm)
!
            matrice_salt(inds+4) = saltij
            matrice_salt(indi+4) = saltij
            if (saltij .gt. samax) then
                samax = saltij
                sm = smm
            endif
!
 10     continue
!
 20 end do
!
! --- CALCUL DU FACTEUR D'USAGE
!
    if (seisme) then
        call rc36fs(nbsig2, nb_occurr, impr_situ, nbsig2, nb_occurr,&
                    impr_situ, matrice_salt, ns, nscy, matse,&
                    mse, matrice_sn, nommat, c, k,&
                    cara, ug)
    else
        if (npass .eq. 0) then
            call rc36fu(nbsig2, nb_occurr, impr_situ, matrice_salt, nommat,&
                        ug, factus)
        else
            call rc36fp(nbsig2, nb_occurr, impr_situ, zi(jnsg), matrice_salt,&
                        nommat, ug, factus)
        endif
    endif
!
    utot = utot + ug
!
    AS_DEALLOCATE(vr=matrice_salt)
    AS_DEALLOCATE(vr=matrice_sn)
    AS_DEALLOCATE(vi=nb_occurr)
    AS_DEALLOCATE(vi=impr_situ)
!
    1000 format ('=> GROUPE: ',i4,' , NOMBRE DE SITUATIONS: ',i4)
    1002 format ('=> LISTE DES SITUATIONS: ',100 (i4,1x))
    1010 format (1p,' SITUATION ',i4,' SN =',e12.5)
    1020 format (1p,' COMBINAISON DES SITUATIONS ',i4,3x,i4,'  SN =',e12.5)
    1031 format (1p,26x,'ETAT_A ETAT_A ',' SP =',e12.5)
    1032 format (1p,26x,'ETAT_B ETAT_A ',' SP =',e12.5)
    1033 format (1p,26x,'ETAT_B ETAT_B ',' SP =',e12.5)
    1034 format (1p,26x,'ETAT_A ETAT_B ',' SP =',e12.5)
!
    1131 format (1p,26x,'ETAT_A ETAT_A ',' SPMECA=',e12.5,' SPTHER=',e12.5)
    1132 format (1p,26x,'ETAT_B ETAT_A ',' SPMECA=',e12.5,' SPTHER=',e12.5)
    1133 format (1p,26x,'ETAT_B ETAT_B ',' SPMECA=',e12.5,' SPTHER=',e12.5)
    1134 format (1p,26x,'ETAT_A ETAT_B ',' SPMECA=',e12.5,' SPTHER=',e12.5)
!
    call jedema()
end subroutine
