subroutine op0027()
! ----------------------------------------------------------------------
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
!     GENE_MATR_ALEA : GENERATEUR DE MATRICES GENERALISEE ALEATOIRE
!     CONSTRUITE EN UTILISANT LE PRINCIPE DU MAXIMUM D'ENTROPIE ET
!     L'INFORMATION DISPONIBLE POUR DES MATRICES GENERALISEES
!     SYMETRIQUES DEFINIES POSITIVES.
!
! ----------------------------------------------------------------------
    implicit none
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/iniran.h"
#include "asterfort/copisd.h"
#include "asterfort/gematg.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/infmaj.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: iret, n1, n, i, m, iak, iadr, iadr1, iadr2
    integer :: idesc, ialime, iaconl, jrefa2, jrefa1
    integer :: jump, iret2
    real(kind=8) :: delta
    character(len=8) :: nomres, nommat
    character(len=16) :: nomcmd, concep
! DEB ------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
!
    call getres(nomres, concep, nomcmd)
!
!
    call getvid(' ', 'MATR_MOYEN', scal=nommat, nbret=n1)
!
    call getvis(' ', 'INIT_ALEA', scal=jump, nbret=n1)
    if (n1 .ne. 0) call iniran(jump)
!
    if (concep .eq. 'MATR_ASSE_GENE_R') then
!
!===================================================
! --- GENERATION D UNE MATRICE GENERALISEE ALEATOIRE
!===================================================
!
        call getvr8(' ', 'COEF_VAR', scal=delta, nbret=n1)
!
        call jeveuo(nommat//'           .DESC', 'L', idesc)
!
! --- VERIF PROFIL=PLEIN
        if (zi(idesc+2) .ne. 2) then
            call utmess('F', 'ALGORITH9_18')
        endif
!
        n = zi(idesc+1)
        m = n*(n+1)/2
!
        call jeexin(nomres//'           .VALM', iret)
!
        if (iret .eq. 0) then
            call jeveuo(nommat//'           .REFA', 'L', jrefa1)
!
! ------ CREATION DES BASES DE DONNEES DE LA MATRICE A GENERER.
!        SUIVANT LE MODELE DE OP0071
!
            call jecrec(nomres//'           .VALM', 'G V R', 'NU', 'DISPERSE', 'CONSTANT',&
                        1)
            call jecroc(jexnum(nomres//'           .VALM', 1))
            call jeecra(nomres//'           .VALM', 'LONMAX', m)
!
!
            call wkvect(nomres//'           .DESC', 'G V I', 3, idesc)
            zi(idesc) = 2
            zi(idesc+1) = n
            zi(idesc+2) = 2
!
            call wkvect(nomres//'           .LIME', 'G V K24', 1, ialime)
            zk24(ialime) = '                        '
!
            call wkvect(nomres//'           .CONL', 'G V R', n, iaconl)
            do 10 i = 1, n
                zr(iaconl+i-1) = 1.0d0
10          continue
!
            call wkvect(nomres//'           .REFA', 'G V K24', 11, jrefa2)
            zk24(jrefa2-1+11)='MPI_COMPLET'
            zk24(jrefa2-1+1) = zk24(jrefa1-1+1)
            zk24(jrefa2-1+2) = zk24(jrefa1-1+2)
            zk24(jrefa2-1+9) = zk24(jrefa1-1+9)
            zk24(jrefa2-1+10) = zk24(jrefa1-1+10)
!
        endif
!
        call jeveuo(jexnum(nommat//'           .VALM', 1), 'L', iak)
        call jeveuo(jexnum(nomres//'           .VALM', 1), 'E', iadr)
        do 20 i = 1, m
            zr(iadr-1+i) = 0.d0
20      continue
!
        call wkvect('&&OP0027.VECTTRA1', 'V V R', m, iadr1)
        call wkvect('&&OP0027.VECTTRA2', 'V V R', m, iadr2)
!
        call gematg(n, delta, zr(iak), zr(iadr), zr(iadr1),&
                    zr(iadr2))
!
!
    else
!===================================================
! --- GENERATION D UN MACRO-ELEMENT DYNAMIQUE ALEATOIRE
!===================================================
!
        call jeveuo(nommat//'.MAEL_RAID_DESC', 'L', idesc)
!
        n = zi(idesc+1)
        m = n*(n+1)/2
        call wkvect('&&OP0027.VECTTRA1', 'V V R', m, iadr1)
        call wkvect('&&OP0027.VECTTRA2', 'V V R', m, iadr2)
!
! -- EXISTENCE DES MATRICES, PRESENCE AMORTISSEMENT, COPIE STUCTURES
        call jeexin(nomres//'.MAEL_RAID_VALE', iret)
        if (iret .eq. 0) then
            call copisd(' ', 'G', nommat, nomres)
        else
            call jeexin(nommat//'.MAEL_AMOR_VALE', iret2)
            if (iret2 .ne. 0) then
                call jeexin(nomres//'.MAEL_AMOR_VALE', iret2)
                if (iret2 .eq. 0) then
                    call copisd(' ', 'G', nommat, nomres)
                endif
            endif
        endif
!
! -- RAIDEUR
        call getvr8(' ', 'COEF_VAR_RIGI', scal=delta, nbret=n1)
        if (delta .gt. 0.d0) then
! GENRRATION RAIDEUR
!
            call jeveuo(nommat//'.MAEL_RAID_VALE', 'L', iak)
            call jeveuo(nomres//'.MAEL_RAID_VALE', 'E', iadr)
            do 30 i = 1, m
                zr(iadr-1+i) = 0.d0
30          continue
!
            call gematg(n, delta, zr(iak), zr(iadr), zr(iadr1),&
                        zr( iadr2))
!
        else
! COPIE VALEURS RAIDEUR
            if (iret .ne. 0) then
                call jeveuo(nommat//'.MAEL_RAID_VALE', 'L', iak)
                call jeveuo(nomres//'.MAEL_RAID_VALE', 'E', iadr)
                do 35 i = 1, m
                    zr(iadr-1+i) = zr(iak-1+i)
35              continue
            endif
!
        endif
!
! -- MASSE
        call getvr8(' ', 'COEF_VAR_MASS', scal=delta, nbret=n1)
!
        if (delta .gt. 0.d0) then
! GENRRATION MASSE
!
            call jeveuo(nommat//'.MAEL_MASS_VALE', 'L', iak)
            call jeveuo(nomres//'.MAEL_MASS_VALE', 'E', iadr)
            do 40 i = 1, m
                zr(iadr-1+i) = 0.d0
40          continue
!
            call gematg(n, delta, zr(iak), zr(iadr), zr(iadr1),&
                        zr( iadr2))
!
        else
! COPIE VALEURS MASSE
            if (iret .ne. 0) then
                call jeveuo(nommat//'.MAEL_MASS_VALE', 'L', iak)
                call jeveuo(nomres//'.MAEL_MASS_VALE', 'E', iadr)
                do 45 i = 1, m
                    zr(iadr-1+i) = zr(iak-1+i)
45              continue
            endif
!
        endif
!
! -- AMORTISSEMNT
        call getvr8(' ', 'COEF_VAR_AMOR', scal=delta, nbret=n1)
!
        if (delta .gt. 0.d0) then
! GENRRATION AMORTISSEMENT
            call jeexin(nommat//'.MAEL_AMOR_VALE', iret)
            if (iret .eq. 0) then
                call utmess('A', 'ALGORITH9_19')
            else
!
                call jeveuo(nommat//'.MAEL_AMOR_VALE', 'L', iak)
                call jeveuo(nomres//'.MAEL_AMOR_VALE', 'E', iadr)
                do 50 i = 1, m
                    zr(iadr-1+i) = 0.d0
50              continue
!
                call gematg(n, delta, zr(iak), zr(iadr), zr(iadr1),&
                            zr(iadr2))
!
            endif
!
        else
! COPIE VALEURS AMORTISSEMENT
            if (iret .ne. 0) then
                call jeexin(nommat//'.MAEL_AMOR_VALE', iret)
                if (iret .ne. 0) then
                    call jeveuo(nommat//'.MAEL_AMOR_VALE', 'L', iak)
                    call jeveuo(nomres//'.MAEL_AMOR_VALE', 'E', iadr)
                    do 55 i = 1, m
                        zr(iadr-1+i) = zr(iak-1+i)
55                  continue
                endif
            endif
        endif
!
    endif
!
!
    call jedema()
end subroutine
