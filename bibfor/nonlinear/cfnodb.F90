subroutine cfnodb(char)
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
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfl.h"
#include "asterfort/utlisi.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    character(len=8) :: char
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - LECTURE DONNEES - ELIMINATION)
!
! DETECTION DE NOEUDS APPARTENANT A DEUX SURFACES DE CONTACT
!
! DEUX CAS SONT DETECTES : AU SEIN D'UNE MEME ZONE
!                          ENTRE 2 SURFACES ESCLAVES
!
! ----------------------------------------------------------------------
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
!
!
!
!
    character(len=24) :: defico
    logical :: lcalc
    integer :: nzoco, nnoco, iform
    character(len=24) :: nodbl, nodbl2
    integer :: jnodbl, jnodb2
    character(len=24) :: contno, sansno, psans
    integer :: jnoco, jsans, jpsans
    integer :: izone, ibid, ndoubl, nvdbl
    integer :: izonea, izoneb, nvdba, nvdbb
    integer :: nbnoe, jdecne, nbnoea, nbnoeb
    integer :: nbnom, jdecnm, jdecea, jdeceb
    integer :: nsans, jdecs, nsansa, jdecsa, nsansb, jdecsb
    integer :: vali(3)
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    defico = char(1:8)//'.CONTACT'
    nzoco = cfdisi(defico,'NZOCO' )
    nnoco = cfdisi(defico,'NNOCO' )
    iform = cfdisi(defico,'FORMULATION')
!
! --- OBJETS TEMPORAIRES
!
    nodbl = '&&CFNODB.NODBL'
    call wkvect(nodbl, 'V V I', nnoco, jnodbl)
    nodbl2 = '&&CFNODB.NODBL2'
    call wkvect(nodbl2, 'V V I', nnoco, jnodb2)
!
! --- ACCES AU TABLEAU DES NOEUDS DE CONTACT
!
    contno = defico(1:16)//'.NOEUCO'
    call jeveuo(contno, 'L', jnoco)
    sansno = defico(1:16)//'.SSNOCO'
    call jeveuo(sansno, 'L', jsans)
    psans = defico(1:16)//'.PSSNOCO'
    call jeveuo(psans, 'L', jpsans)
!
! ----------------------------------------------------------------------
!
! --- PREMIER CAS : NOEUDS COMMUNS DANS UNE MEME ZONE DE CONTACT
!
    do 100 izone = 1, nzoco
        nbnoe = mminfi(defico,'NBNOE' ,izone )
        nbnom = mminfi(defico,'NBNOM' ,izone )
        jdecne = mminfi(defico,'JDECNE',izone )
        jdecnm = mminfi(defico,'JDECNM',izone )
        lcalc = mminfl(defico,'CALCUL',izone )
        if (.not.lcalc) then
            goto 100
        endif
        call utlisi('INTER', zi(jnoco+jdecne), nbnoe, zi(jnoco+jdecnm), nbnom,&
                    zi(jnodbl), nnoco, ndoubl)
        if (ndoubl .ne. 0) then
            if (ndoubl .gt. 0) then
! --------- LES NOEUDS COMMUNS SONT-ILS EXCLUS PAR SANS_NOEUD ?
                nsans = zi(jpsans+izone) - zi(jpsans+izone-1)
                jdecs = zi(jpsans+izone-1)
                call utlisi('DIFFE', zi(jnodbl), ndoubl, zi(jsans+ jdecs), nsans,&
                            ibid, 1, nvdbl)
! --------- NON !
                if (nvdbl .ne. 0) then
                    vali(1) = izone
                    vali(2) = abs(nvdbl)
                    call utmess('F', 'CONTACT2_13', ni=2, vali=vali)
                endif
            else
                ASSERT(.false.)
            endif
        endif
100  end do
!
! ----------------------------------------------------------------------
!
! --- SECOND CAS : NOEUDS COMMUNS A DEUX SURFACES ESCLAVES
!
    do 200 izonea = 1, nzoco
        lcalc = mminfl(defico,'CALCUL',izonea)
        if (.not.lcalc) then
            goto 200
        endif
        do 201 izoneb = izonea+1, nzoco
            lcalc = mminfl(defico,'CALCUL',izoneb)
            if (.not.lcalc) then
                goto 201
            endif
            nbnoea = mminfi(defico,'NBNOE' ,izonea)
            nbnoeb = mminfi(defico,'NBNOE' ,izoneb)
            jdecea = mminfi(defico,'JDECNE',izonea)
            jdeceb = mminfi(defico,'JDECNE',izoneb)
            call utlisi('INTER', zi(jnoco+jdecea), nbnoea, zi(jnoco+ jdeceb), nbnoeb,&
                        zi(jnodbl), nnoco, ndoubl)
            if (ndoubl .ne. 0) then
                if (ndoubl .gt. 0) then
                    if (iform .eq. 1) then
! ------------- LES NOEUDS COMMUNS SONT-ILS EXCLUS PAR LA ZONE A ?
                        nsansa = zi(jpsans+izonea) - zi(jpsans+izonea- 1)
                        jdecsa = zi(jpsans+izonea-1)
                        call utlisi('DIFFE', zi(jnodbl), ndoubl, zi(jsans+jdecsa), nsansa,&
                                    zi(jnodb2), nnoco, nvdba)
                        if (nvdba .ne. 0) then
                            if (nvdba .gt. 0) then
! ----------------- LES NOEUDS RESTANTS SONT-ILS EXCLUS PAR LA ZONE B ?
                                nsansb = zi(jpsans+izoneb) - zi( jpsans+izoneb-1)
                                jdecsb = zi(jpsans+izoneb-1)
                                call utlisi('DIFFE', zi(jnodb2), nvdba, zi(jsans+jdecsb), nsansb,&
                                            ibid, 1, nvdbb)
                                if (nvdbb .ne. 0) then
                                    vali(1) = izonea
                                    vali(2) = izoneb
                                    vali(3) = abs(nvdbb)
                                    call utmess('A', 'CONTACT2_15', ni=3, vali=vali)
                                endif
                            else
                                ASSERT(.false.)
                            endif
                        endif
                    else if (iform.eq.2) then
                        vali(1) = izonea
                        vali(2) = izoneb
                        vali(3) = abs(ndoubl)
                        call utmess('F', 'CONTACT2_16', ni=3, vali=vali)
                    else
                        ASSERT(.false.)
                    endif
                else
                    ASSERT(.false.)
                endif
            endif
201      continue
200  end do
!
! --- MENAGE
!
    call jedetr(nodbl)
    call jedetr(nodbl2)
!
    call jedema()
end subroutine
