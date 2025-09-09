package org.shark.renovatio.provider.cobol.domain;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.Details.FieldDetail;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Adapter implementation that converts jRecord {@link AbstractLine} datasets
 * into simple {@link Map} structures that can later be persisted in a
 * database or object store.
 */
public class JRecordDatasetAdapter implements DatasetAdapter<AbstractLine> {

    @Override
    public Map<String, Object> toPersistable(AbstractLine line) {
        Map<String, Object> record = new LinkedHashMap<>();
        if (line == null) {
            return record;
        }
        LayoutDetail layout = line.getLayout();
        RecordDetail recordDetail = layout.getRecord(0);
        for (int i = 0; i < recordDetail.getFieldCount(); i++) {
            FieldDetail field = recordDetail.getField(i);
            record.put(field.getName(), line.getFieldValue(field).asString());
        }
        return record;
    }
}
