subroutine inclis(nomres, ssta, sstb, intfa, intfb,&
                  fmlia, fplian, fplibn, fpliao, fplibo,&
                  iada, iadb, numlis, matprj)
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
!***********************************************************************
!  O. NICOLAS     DATE 01/08/04
!-----------------------------------------------------------------------
!  BUT : < CALCUL DES LIAISONS CAS INCOMPATIBLE>
!
!  CALCULER LES NOUVELLES MATRICES DE LIAISON EN TENANT COMPTE DE
!  L'ORIENTATION DES SOUS-STRUCTURES ET DES INCOMPATIBILITES
!  ON DETERMINE LES MATRICES DE LIAISON, LES DIMENSIONS DE CES MATRICES
!  ET LE PRONO ASSOCIE
!
!-----------------------------------------------------------------------
!
! NOMRES  /I/ : NOM K8 DU MODELE GENERALISE
! SSTA    /I/ : NOM K8 DE LA SOUS-STRUCTURE MAITRE
! SSTB    /I/ : NOM K8 DE LA SOUS-STRUCTURE ESCLAVE
! INTFA   /I/ : NOM K8 DE L'INTERFACE DE SSTA
! INTFB   /I/ : NOM K8 DE L'INTERFACE DE SSTB
! FPLIAO /I/ : FAMILLE DES PROFNO MATRICES DE LIAISON ORIENTEES SSTA
! FPLIAN /I/ : FAMILLE DES PROFNO MATRICES DE LIAISON NON ORIENTEES SSTA
! FPLIBO /I/ : FAMILLE DES PROFNO MATRICES DE LIAISON ORIENTEES SSTB
! FPLIBN /I/ : FAMILLE DES PROFNO MATRICES DE LIAISON NON ORIENTEES SSTB
! IADA   /I/ : VECTEUR DES CARACTERISTIQUES LIAISON SSTA
! IADB   /I/ : VECTEUR DES CARACTERISTIQUES LIAISON SSTB
! NUMLIS /I/ : NUMERO INTERFACE COURANTE
! MATPRJ /I/ : NOM K8 DE LA MATRICE D'OBSERVATION INTERFACE
!               MAITRE/ESCLAVE
! FMLIA   /I/ : FAMILLE DES MATRICES DE LIAISON
!
!
!
!
!
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/isdeco.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/rotlis.h"
#include "asterfort/utmess.h"
!
    character(len=8) :: nomres, matprj, ssta, sstb, intfa, intfb, nomg
    character(len=24) :: fmlia, toto, fpliao, fplibo, fplian, fplibn
    integer :: iada(3), iadb(3), numlis, zit(3), nbec, nbnoea, nbnoeb
    integer :: nbcmpm, k, m1, n1, m2, n2, llplia, llplib, icompa, icompb, ldmat
    integer :: ldmat2, iadoa, iadob
    parameter      (nbcmpm=10)
    integer :: idecoa(nbcmpm), idecob(nbcmpm), itemcm
    real(kind=8) :: rbid, un, moins1
!
!-----------------------------------------------------------------------
    data un,moins1 /1.0d+00,-1.0d+00/
!-----------------------------------------------------------------------
!
    call jemarq()
!
    toto='TATA'
    nomg = 'DEPL_R'
    call dismoi('NB_EC', nomg, 'GRANDEUR', repi=nbec)
    if (nbec .gt. 10) then
        call utmess('F', 'MODELISA_94')
    endif
!
    call jeveuo(matprj, 'L', itemcm)
!
! Calcul de la matrice orientee de la structure esclave
    if (iadb(3) .lt. iada(3)) then
        call rotlis(nomres, fmlia, iadb, fplibn, fplibo,&
                    numlis, sstb, intfb, un)
    endif
! Calcul de la matrice orientee de la structure maitre
    zit(1)=iada(1)
    zit(2)=iada(2)
    zit(3)=1
    call jecrec(toto, 'V V R', 'NU', 'DISPERSE', 'VARIABLE',&
                1)
!
    call rotlis(nomres, toto, zit, fplian, fpliao,&
                numlis, ssta, intfa, moins1)
    call jecroc(jexnum(fmlia, iada(3)))
    call jeecra(jexnum(fmlia, iada(3)), 'LONMAX', iadb(1)*iada(2))
    call jeveuo(jexnum(fmlia, iada(3)), 'E', ldmat)
    call jeveuo(jexnum(toto, 1), 'L', ldmat2)
!
! Recuperation des donnees composantes
    call jeveuo(jexnum(fpliao, numlis), 'L', llplia)
    call jelira(jexnum(fpliao, numlis), 'LONMAX', nbnoea)
    nbnoea=nbnoea/(1+nbec)
    call jeveuo(jexnum(fplibo, numlis), 'L', llplib)
    call jelira(jexnum(fplibo, numlis), 'LONMAX', nbnoeb)
    nbnoeb=nbnoeb/(1+nbec)
!
!
! boucle sur nombre de mode de la structure maitre
    do k = 1, iada(2)
! boucle sur nombre de noeuds d'interface de la structure esclave
        do m1 = 1, nbnoeb
            iadob=zi(llplib+(m1-1)*(1+nbec))
            call isdeco(zi(llplib+(m1-1)*(1+nbec)+1), idecob, nbcmpm)
            icompb=iadob-1
! boucle sur nombre de composante de la structure esclave
            do n1 = 1, nbcmpm
                if (idecob(n1) .gt. 0) then
! boucle sur nombre de noeuds d'interface de la structure maitre
                    icompb=icompb+1
                    rbid=0.d0
                    do m2 = 1, nbnoea
                        iadoa=zi(llplia+(m2-1)*(1+nbec))
                        call isdeco(zi(llplia+(m2-1)*(1+nbec)+1), idecoa, nbcmpm)
! boucle sur nombre de composante de la structure maitre
                        icompa=iadoa-1
                        do n2 = 1, nbcmpm
                            if ((idecoa(n2).gt.0) .and. (n1.eq.n2)) then
                                icompa=icompa+n2
                                rbid=rbid+ zr(itemcm+(icompb-1)*iada(&
                                1)+icompa-1)* zr(ldmat2+(k-1)*iada(1)+&
                                icompa-1)
                            endif
                        end do
                    end do
                    zr(ldmat+(k-1)*iadb(1)+icompb-1)=rbid
                endif
            end do
        end do
    end do
! On corrige in fine la taille de la nouvelle matrice de liaison
    iada(1)=iadb(1)
    call jedetr(toto)
!
    if (iadb(3) .gt. iada(3)) then
        call rotlis(nomres, fmlia, iadb, fplibn, fplibo,&
                    numlis, sstb, intfb, un)
    endif
!
!
    call jedema()
end subroutine
