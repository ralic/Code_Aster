subroutine matcod(chmat, indmat, nbmat, imate, igrp,&
                  materi, codi)
    implicit none
#include "jeveux.h"
#include "asterc/isnnem.h"
#include "asterfort/alfint.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/exisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedupc.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveut.h"
#include "asterfort/jexatr.h"
#include "asterfort/tbexlr.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: chmat, materi
    character(len=19) :: codi
    integer :: indmat, nbmat, imate, igrp
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
! person_in_charge: j-pierre.lefebvre at edf.fr
!-----------------------------------------------------------------------
!     MATERIAU CODE APPELE PAR RCMACO ET PMMACO
!-----------------------------------------------------------------------
!
!     BUT: CREER L'OBJET NOMMAT//'      .CODI' ,LE REMPLIR ET RENVOYER
!          SON ADRESSE PAR RAPPORT A ZI
!
! IN  CHMAT  : NOM DU CHAM_MATER POUR ALFINT
! IN  INDMAT : INDICE DU PREMIER MATERIAU DANS LA LISTE
! IN  NBMAT  : NOMBRE DE MATERIAUX DU NUMERO D'OCCURRENCE DE AFFE
! IN  IMATE  : NUMERO DU GROUPE
! IN  IGRP   : ADRESSE DU VECTEUR K8 CONTENANT LES NOMS DES MATERIAUX
! IN  NOMMAT : NOM DU MATERIAU
! OUT  CODI   : OBJET MATERIAU CODE
!    CODI(1)   : ADRESSE ZK16  DE '.MATERIAU.NOMRC'
!    CODI(2)   : NOMBRE DE TYPES DE COMPORTEMENT
!
!         P.I = CODI(2+I)
!
!    CODI(2+I)  :POINTEUR DANS .CODI DU IEME COMPORTEMENT
!    CODI(P.I)  :NOMBRE DE COEFFICIENTS REELS
!    CODI(P.I+1):NOMBRE DE COEFFICIENTS COMPLEXES
!    CODI(P.I+2):NOMBRE DE COEFFICIENTS FONCTIONS
!    CODI(P.I+3):ADRESSE ZK8 RELATIVE AU .VALK DES PARAMETRES (NOMS)
!    CODI(P.I+4):ADRESSE ZR  RELATIVE AU .VALR DES REELS
!    CODI(P.I+5):ADRESSE ZC  RELATIVE AU .VALC DES COMPLEXES
!
!         P.IF = P.I+6
!
!    CODI(P.IF+LFCT*(K-1))  :NOMBRE DE POINTS DE LA FONCTION ASSOCIEE
!    CODI(P.IF+LFCT*(K-1)+1):ADRESSE ZK16 DU .PROL
!    CODI(P.IF+LFCT*(K-1)+2):ADRESSE ZR  DU .VALE
!    CODI(P.IF+LFCT*(K-1)+3):ADRESSE ZI  DU POINTEUR DE LONGUEUR(NAPPE)
!    CODI(P.IF+LFCT*(K-1)+4):ADRESSE ZR  DU .PARA (NAPPE)
!    CODI(P.IF+LFCT*(K-1)+5):LONUTI  DU .PARA (NAPPE)
!    CODI(P.IF+LFCT*(K-1)+6):POINTEUR SUPPLEMENTAIRE (TRACTION,TRC)
!    CODI(P.IF+LFCT*(K-1)+7):INDICE DE L'INTERVALLE POUR INTERPOLATION
!    CODI(P.IF+LFCT*(K-1)+8):INDICE SUPPLEMENTAIRE
!
!         P.IFC = CODI(P.IF+LFCT*(K-1)+6))
!
!    CODI(P.IFC)  :ADRESSE ZK8 DU .&&RDEP.PROL
!    CODI(P.IFC+1):ADRESSE ZR  DU .&&RDEP.VALE
!
! ----------------------------------------------------------------------
!
!
!
    integer :: iret, iretf, irett, nbcm, jnomrc, lmat, lfct, lsup
    integer :: jnbcm, l, nbv, nbtt, nbcot, nbcmt, nbco
    integer :: nbt, jdim, k, kk, nbk, lgcodi, isundf, idma
    integer :: imat, ipi, ipif, nbpts, ipifc, m, iretc
    integer :: jcodi, jnomr, jjdim, jlcod, ipi0
    real(kind=8) :: tdef, prec
    character(len=4) :: knuma1
    character(len=3) :: knuma2
    character(len=3) :: knuma3
    character(len=8) :: nopara, nommat
    character(len=19) :: ch19, chma, listr
! ----------------------------------------------------------------------
! PARAMETER ASSOCIE AU MATERIAU CODE
!
! --- LMAT   : NOMBRE DE PARAMETRES ASSOCIES AU COMPORTEMENT
! --- LFCT   : NOMBRE DE PARAMETRES ASSOCIES AUX FONCTIONS
! --- LSUP   : NOMBRE DE PARAMETRES SUPPLEMENTAIRE (COURBE &&RDEP)
    parameter        ( lmat = 7 , lfct = 9 , lsup = 2 )
! ----------------------------------------------------------------------
!
    call jemarq()
!
    call codent(imate, 'D0', knuma1)
    codi = ' '
    codi(1:8) = materi
    codi(9:13) = '.'//knuma1
    call jeexin(codi//'.CODI', iretc)
    if (iretc .ne. 0) then
        call jeveut(codi//'.CODI', 'L', jcodi)
        goto 999
    endif
!
    call wkvect('&&RCMACO.NBCM', 'V V I', nbmat, jnbcm)
    call wkvect('&&RCMACO.NOMR', 'V V I', nbmat, jnomr)
    call wkvect('&&RCMACO.JDIM', 'V V I', nbmat, jjdim)
    call wkvect('&&RCMACO.LCOD', 'V V I', nbmat, jlcod)
    do 100 l = 1, nbmat
        nommat=zk8(igrp+indmat+l-1)
        call jeexin(nommat//'.MATERIAU.NOMRC', iret)
        ASSERT(iret .ne. 0)
        call jelira(nommat//'.MATERIAU.NOMRC', 'LONMAX', zi(jnbcm+l-1))
        call jeveut(nommat//'.MATERIAU.NOMRC', 'L', zi(jnomr+l-1))
        nbv = 0
        if (zk16(zi(jnomr+l-1)) .eq. 'ELAS_COQMU') nbv = 1
        if (zk16(zi(jnomr+l-1)+nbv) .eq. 'THER_COQMU') nbv = nbv+1
        if (nbv .gt. 0) zi(jnbcm+l-1) = nbv
100  end do
!
    nbtt = 0
    nbcot = 0
    nbcmt=0
    do 200 l = 1, nbmat
        nbco = 0
        nbt = 0
        nommat=zk8(igrp+indmat+l-1)
        nbcm=zi(jnbcm+l-1)
        jnomrc=zi(jnomr+l-1)
        call codent(l, 'D0', knuma3)
        call jedetr('&&RCMACO.DIM'//knuma3)
        call wkvect('&&RCMACO.DIM'//knuma3, 'V V I', lmat*nbcm, zi(jjdim+l-1))
        jdim=zi(jjdim+l-1)
        do 10 k = 1, nbcm
            kk = jdim+lmat*(k-1)
            chma = nommat//'.'//zk16(jnomrc+k-1)(1:10)
            call codent(k, 'D0', knuma2)
            ch19 = chma(1:8)//'.'//knuma2//knuma1//knuma3
            call jedupc(' ', chma, 1, 'V', ch19,&
                        .false.)
            call jelira(ch19//'.VALR', 'LONUTI', zi(kk))
            call jeveut(ch19//'.VALR', 'L', zi(kk+1))
            call jelira(ch19//'.VALC', 'LONUTI', zi(kk+2))
            call jeveut(ch19//'.VALC', 'L', zi(kk+3))
            call jelira(ch19//'.VALK', 'LONUTI', nbk)
            call jeveut(ch19//'.VALK', 'L', zi(kk+5))
            zi(kk+4) = ( nbk - zi(kk) - zi(kk+2) ) / 2
            nbco = nbco + zi(kk+4)
            if ((zk16(jnomrc+k-1)(1:8) .eq. 'TRACTION')) then
                zi(kk+6) = 1
                nbt = nbt + 1
            endif
            if (zk16(jnomrc+k-1)(1:13) .eq. 'META_TRACTION') then
                zi(kk+6) = 1
                nbt = nbt + nbk/2
            endif
10      continue
        zi(jlcod+l-1)=2 + lmat*nbcm+ lfct*nbco + lsup*nbt
        nbcmt=nbcmt+nbcm
        nbtt = nbtt + nbt
        nbcot = nbcot + nbco
200  end do
!
    lgcodi= 2*nbmat+1+ 2*nbmat + lmat*nbcmt + lfct*nbcot + lsup*nbtt
    call wkvect(codi//'.CODI', 'V V I', lgcodi, jcodi)
    call jeveut(codi//'.CODI', 'E', jcodi)
    isundf = isnnem()
    do 12 k = 1, lgcodi
        zi(jcodi + k-1) = isundf
12  end do
    zi(jcodi ) = nbmat
    ipi0=2*nbmat+1
    idma=ipi0
    do 300 imat = 1, nbmat
        nommat=zk8(igrp+indmat+imat-1)
        nbcm=zi(jnbcm+imat-1)
        jnomrc=zi(jnomr+imat-1)
        jdim=zi(jjdim+imat-1)
        zi(jcodi+imat)=igrp+indmat+imat-1
        zi(jcodi+imat+nbmat)=idma
!
        zi(jcodi+idma ) = jnomrc
        nbcm=zi(jnbcm+imat-1)
        zi(jcodi+idma+1) = nbcm
        ipi = jcodi+idma+2+nbcm
!
        do 20 k = 1, nbcm
!
            chma = nommat//'.'//zk16(jnomrc+k-1)(1:10)
!
            kk = jdim+lmat*(k-1)
            zi(jcodi+idma+1+k) = ipi
            zi(ipi) = zi(kk)
            zi(ipi+1) = zi(kk+2)
            zi(ipi+2) = zi(kk+4)
            zi(ipi+3) = zi(kk+5)
            zi(ipi+4) = zi(kk+1)
            zi(ipi+5) = zi(kk+3)
            ipif = ipi+lmat-1
!
! ---     BOUCLE SUR LE NOMBRE DE COEFFICIENTS REELS :
!         ------------------------------------------
            do 21 l = 0, zi(kk)-1
                ch19 = zk8(zi(kk+5)+l)
                if (ch19 .eq. 'PRECISIO') prec = zr(zi(kk+1)+l)
21          continue
            do 22 l = 0, zi(kk)-1
                ch19 = zk8(zi(kk+5)+l)
                if (ch19 .eq. 'TEMP_DEF') then
                    tdef = zr(zi(kk+1)+l)
!
! ---       BOUCLE SUR LES FONCTIONS :
!           ------------------------
                    do 23 m = 0, zi(kk+4)-1
                        ch19 = zk8(zi(kk+5)+zi(kk)+zi(kk+2)+zi(kk+4)+ m)
                        nopara = zk8(zi(kk+5)+zi(kk)+zi(kk+2)+m)
                        if (nopara(1:5) .eq. 'ALPHA' .or. nopara .eq. 'F_ALPHA ' .or.&
                            nopara .eq. 'C_ALPHA ') then
!
! ---        INTERPOLATION DES COEFFICIENTS DE DILATATION ALPHA
! ---        EN TENANT COMPTE DE LA TEMPERATURE DE DEFINITION TDEF :
!            -----------------------------------------------------
                            if (chmat .ne. ' ') then
                                call alfint(chmat, imate, nommat, tdef, nopara,&
                                            k, prec, ch19)
                                zk8(zi(kk+5)+zi(kk)+zi(kk+2)+zi(kk+4)+&
                                m) = ch19
                            endif
                        endif
23                  continue
!
                endif
22          continue
!
! ---     BOUCLE SUR LE NOMBRE DE COEFFICIENTS FONCTIONS :
!         ------------------------------------------------
            do 25 l = 0, zi(kk+4)-1
                ch19 = zk8(zi(kk+5)+zi(kk)+zi(kk+2)+zi(kk+4)+l)
                call exisd('FONCTION', ch19(1:8), iretf)
                call exisd('TABLE', ch19(1:8), irett)
! ---   DES FONCTIONS SONT CREEES SUR LA VOLATILE (ROUTINE ALFINT) ---
                if (iretf .eq. 1) then
                    call jeveut(ch19//'.PROL', 'L', zi(ipif+1))
                    zi(ipif+7) = 1
                    zi(ipif+8) = 1
                    if (zk24(zi(ipif+1))(1:1) .eq. 'C' .or. zk24(zi( ipif+1))(1:1) .eq. 'F') then
                        call jeveut(ch19//'.VALE', 'L', zi(ipif+2))
                        call jelira(ch19//'.VALE', 'LONMAX', nbpts)
                        zi(ipif) = nbpts/2
                    else if (zk24(zi(ipif+1))(1:1) .eq. 'N') then
                        call jeveut(ch19//'.VALE', 'L', zi(ipif+2))
                        call jeveut(jexatr(ch19//'.VALE', 'LONCUM'), 'L', zi(ipif+3))
                        call jeveut(ch19//'.PARA', 'L', zi(ipif+4))
                        call jelira(ch19//'.PARA', 'LONUTI', zi(ipif+ 5))
                    else if (zk24(zi(ipif+1))(1:1) .eq. 'I') then
                    else
                        call utmess('F', 'MODELISA6_64', sk=zk24(zi(ipif+ 1)))
                    endif
                else if (irett .eq. 1) then
                    listr = '&&'//ch19(1:8)//'_LR8'
                    call jeexin(listr//'.VALE', iretc)
                    if (iretc .eq. 0) then
                        call tbexlr(ch19, listr, 'V')
                    endif
                    call jeveut(listr//'.VALE', 'L', zi(ipif))
                    zi(ipif+1) = 0
                    zi(ipif+2) = 0
                else
                    call utmess('F', 'MODELISA6_64', sk=ch19(1:8))
                endif
!
                if (zi(kk+6) .eq. 1) then
                    if (( zk16(jnomrc+k-1)(1:8) .eq. 'TRACTION' ) .or.&
                        (zk16(jnomrc+k-1)(1:13) .eq. 'META_TRACTION')) then
                        ipifc = ipif+lfct
                        zi(ipif+6) = ipifc
                        ch19 = nommat//'.&&RDEP'
                        call jeveut(ch19//'.PROL', 'E', zi(ipifc))
                        call jeveut(ch19//'.VALE', 'E', zi(ipifc+1))
                        ipif = ipifc + lsup
                    else
                        ipif = ipif + lfct
                    endif
                else
                    ipif = ipif + lfct
                endif
25          continue
            ipi = ipif
20      continue
        idma=idma+zi(jlcod+imat-1)
300  end do
!
! --- MENAGE
    do 400 l = 1, nbmat
        call codent(l, 'D0', knuma3)
        call jedetr('&&RCMACO.DIM'//knuma3)
400  end do
    call jedetr('&&RCMACO.NBCM')
    call jedetr('&&RCMACO.NOMR')
    call jedetr('&&RCMACO.JDIM')
    call jedetr('&&RCMACO.LCOD')
!
!
999  continue
    call jedema()
!
end subroutine
